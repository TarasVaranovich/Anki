package edu.evolution.varanovich.anki.api.http.dispatcher

import cats.effect.{ContextShift, IO, Sync}
import doobie.implicits._
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode._
import edu.evolution.varanovich.anki.api.http.AnkiServer._
import edu.evolution.varanovich.anki.api.http.protocol.AnkiRequest.UserRequest
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse._
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.UserProgram._
import edu.evolution.varanovich.anki.model.User
import edu.evolution.varanovich.anki.utility.CryptoUtility.{encryptSHA256, generateToken}
import edu.evolution.varanovich.anki.validator.UserValidator
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser.decode
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.{Request, Response, Status}

object UserDispatcher {
  private implicit def logger[F[_] : Sync] = Slf4jLogger.getLogger[F]

  def doRegister(request: Request[IO], cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    val register: User => IO[Response[IO]] = (user: User) =>
      for {
        token <- IO(generateToken)
        userOpt <- DbManager.transactor.use(readUser(user.name).transact[IO])
          .handleErrorWith((ex: Throwable) =>
            logger[IO].error(ex)("Cannot read user.") *> IO(None))
        saveResult <- userOpt match {
          case Some(_) => IO(AlreadyExists)
          case None => DbManager.transactor.use(createUser(user).transact[IO])
            .handleErrorWith((ex: Throwable) =>
              logger[IO].error(ex)("Cannot save user.") *> IO(ServerError))
        }
        idOpt <- saveResult match {
          case OperationSuccess => DbManager.transactor.use(readUserId(user.name).transact[IO])
            .handleErrorWith((ex: Throwable) =>
              logger[IO].error(ex)("Cannot receive user identifier by name.") *> IO(None))
          case _ => IO(None)
        }
        registerResult <- idOpt match {
          case Some(id) => cache.put(id, UserSession(token, user.privileges, user.name)) *> IO(OperationSuccess)
          case None => IO(saveResult)
        }
      } yield (registerResult, token, idOpt) match {
        case (OperationSuccess, token, idOpt) => idOpt match {
          case Some(id) => Response(Status.Created).withEntity(UserResponse(id, token))
          case None => ServerErrorResponse
        }
        case (AlreadyExists, _, _) => Response(Status.Accepted).withEntity(ErrorResponse("User exists."))
        case (ServerError, _, _) => ServerErrorResponse
        case (_, _, _) =>
          Response(Status.Accepted).withEntity(ErrorResponse("Unknown error. User not created."))
      }
    executeValidated(request, register)
  }

  def doLogin(request: Request[IO], cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    val login: User => IO[Response[IO]] = (user: User) =>
      for {
        token <- IO(generateToken)
        passwordOpt <- DbManager.transactor.use(readPassword(user.name).transact[IO])
          .handleErrorWith((ex: Throwable) => logger[IO].error(ex)("Cannot receive password.") *> IO(None))
        idOpt <- DbManager.transactor.use(readUserId(user.name).transact[IO])
          .handleErrorWith((ex: Throwable) =>
            logger[IO].error(ex)("Cannot receive user identifier by name.") *> IO(None))
        attemptsOpt <- idOpt match {
          case Some(id) => cache.get(id).map(_.map(_.loginAttempts))
          case None => IO(None)
        }
        isLocked <- DbManager.transactor.use(isLockedUser(user).transact[IO])
          .handleErrorWith((ex: Throwable) => logger[IO].error(ex)("Cannot lock user.") *> IO(false))
        loginResult <- (passwordOpt, idOpt, isLocked) match {
          case (Some(password), Some(id), false) => tryLogin(password, token, user, attemptsOpt, id, cache)
          case (_, None, false) => IO(NotExists)
          case (_, _, true) => IO(Blocked)
          case (_, _, _) => IO(ServerError)
        }
      } yield (loginResult, token, idOpt) match {
        case (OperationSuccess, token, Some(id)) => Response(Status.Ok).withEntity(UserResponse(id, token))
        case (NotExists, _, _) => Response(Status.Accepted).withEntity(ErrorResponse("User not found."))
        case (WrongPassword, _, _) => Response(Status.Accepted).withEntity(ErrorResponse("Wrong password."))
        case (Blocked, _, _) => Response(Status.Accepted).withEntity(ErrorResponse("User is blocked."))
        case (_, _, _) =>
          Response(Status.Accepted).withEntity(ErrorResponse("Unknown error. Use are not logged in."))
      }
    executeValidated(request, login)
  }

  private def tryLogin(password: String,
                       token: String,
                       user: User,
                       attemptsOpt: Option[Int],
                       id: String,
                       cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]): IO[Int] =
    if (password == encryptSHA256(user.password))
      cache.put(id, UserSession(token, user.privileges, user.name)) *> IO(OperationSuccess)
    else attemptsOpt match {
      case Some(attempts) => reduceAttempts(user, attempts, id, cache)
      case None => cache.put(id, UserSession("", user.privileges, "")) *> IO(WrongPassword)
    }

  private def reduceAttempts(user: User,
                             attempts: Int,
                             id: String,
                             cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]): IO[Int] =
    if (attempts > 0)
      cache.put(id, UserSession("", user.privileges, user.name, (attempts - 1))) *> IO(WrongPassword)
    else DbManager.transactor.use(lockUser(id).transact[IO])
      .redeem((ex: Throwable) => {
        logger[IO].error(ex)("Cannot lock user.")
        ServerError
      }, (_: Int) => WrongPassword)

  private def executeValidated(request: Request[IO], function: User => IO[Response[IO]]): IO[Response[IO]] =
    request.as[String].flatMap {
      body =>
        decode[UserRequest](body) match {
          case Left(_) =>
            IO(Response(Status.Accepted).withEntity(ErrorResponse("Cannot parse user from request body.")))
          case Right(userData) =>
            UserValidator.validate(userData.name, userData.password, "member").toEither match {
              case Left(errors) =>
                IO(Response(Status.Accepted)
                  .withEntity(MultiErrorResponse(errors.map(_.message).toNonEmptyList.toList.toArray)))
              case Right(user) => function(user)
            }
        }
    }
}