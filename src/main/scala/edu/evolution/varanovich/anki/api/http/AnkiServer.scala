package edu.evolution.varanovich.anki.api.http

import cats.effect.{ExitCode, IO, IOApp}
import doobie.implicits._
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode.{AlreadyExists, ServerError}
import edu.evolution.varanovich.anki.api.http.ServerConfig._
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.UserProgram._
import edu.evolution.varanovich.anki.utility.CryptoUtility.generateToken
import edu.evolution.varanovich.anki.validator.UserValidator
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser.decode
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io.{->, /, GET, POST, Root}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, Response, Status}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

//TODO: extract requests in separate files
object AnkiServer extends IOApp {
  final case class RegisterUserRequest(name: String, password: String)
  final case class RegisterUserResponse(token: String) {
    def message: String = "User created."
  }
  final case class ErrorResponse(message: String)
  final case class MultiErrorResponse(messages: Array[String])

  def ankiRoutes(cache: Cache[IO, String, UserSession]) = {
    HttpRoutes.of[IO] {
      case request@POST -> Root / "register" => {
        request.as[String].flatMap {
          body =>
            decode[RegisterUserRequest](body) match {
              case Left(_) =>
                IO(Response(Status.BadRequest).withEntity(ErrorResponse("Cannot parse user from request body.")))
              case Right(userData) =>
                UserValidator.validate(userData.name, userData.password, "member").toEither match {
                  case Left(errors) =>
                    IO(Response(Status.UnprocessableEntity)
                      .withEntity(MultiErrorResponse(errors.map(_.message).toNonEmptyList.toList.toArray)))
                  case Right(user) => for {
                    token <- IO(generateToken)
                    userExists <- DbManager.transactor.use(readUser(user.name).transact[IO])
                      .handleErrorWith((_: Throwable) => IO(None))
                    saveResult <- if (userExists.isEmpty) cache.put(token, UserSession(user.privileges)) *>
                      DbManager.transactor.use(createUser(user).transact[IO])
                        .handleErrorWith((_: Throwable) => IO(ServerError)) else IO(AlreadyExists)
                  } yield (saveResult, token) match {
                    case (1, token) => Response(Status.Created).withEntity(RegisterUserResponse(token))
                    case (AlreadyExists, _) =>
                      Response(Status.Conflict).withEntity(ErrorResponse("User exists."))
                    case (ServerError, _) =>
                      Response(Status.InternalServerError).withEntity(ErrorResponse("Server error."))
                    case (_, _) =>
                      Response(Status.InternalServerError).withEntity(ErrorResponse("Unknown error. User not created."))
                  }
                }
            }
        }
      }

      case request
        @GET -> Root / "guess" / candidate => ???

      //create user request with token; 1)check if registered yet; 2)check if logged in yet
      //login request with token
    }
  }

  def httpApp(cache: Cache[IO, String, UserSession]) = {
    ankiRoutes(cache)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    cache <- Cache.of[IO, String, UserSession](cacheExpiration.minutes, cacheInvalidation.minutes)
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = port, host = host)
      .withHttpApp(httpApp(cache))
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}