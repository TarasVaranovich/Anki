package edu.evolution.varanovich.anki.api.http.dispatcher

import java.util.UUID

import cats.effect.{IO, Resource}
import doobie.Transactor
import doobie.implicits._
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode.{OperationSuccess, ServerError}
import edu.evolution.varanovich.anki.api.http.AnkiServer.ServerErrorResponse
import edu.evolution.varanovich.anki.api.http.dispatcher.DispatcherUtility.executeAuthenticated
import edu.evolution.varanovich.anki.api.http.protocol.AnkiRequest.CreateAnswerInfoRequest
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.{AnkiGenericResponse, CardsForImproveResponse, ErrorResponse}
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.domain.AnswerInfoProgram
import edu.evolution.varanovich.anki.db.program.domain.CardProgram._
import edu.evolution.varanovich.anki.db.program.domain.DeckProgram._
import edu.evolution.varanovich.anki.model.{AnswerInfo, Card}
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.circe.CirceEntityCodec.{circeEntityEncoder, _}
import org.http4s.{Request, Response, Status}

final case class CardDispatcher(logger: SelfAwareStructuredLogger[IO])(
  implicit transactor: Resource[IO, Transactor[IO]], cache: Cache[IO, String, UserSession]) {
  def createAnswerInfo(request: Request[IO]): IO[Response[IO]] = {
    val createInfo: String => IO[Response[IO]] = (userId: String) => {
      val createAnswerInfoInDatabase: (String, Card, AnswerInfo) => IO[Response[IO]] =
        (description: String, card: Card, info: AnswerInfo) =>
          for {
            userNameOpt <- cache.get(userId).map(_.map(_.userName))
            deckIdOpt <- userNameOpt match {
              case Some(name) => runTransaction(readDeckIdByDescriptionAndUserName(description, name))
                .handleErrorWith((ex: Throwable) =>
                  logger.error(ex)("Cannot find deck by description and user.") *> IO(None))
              case None => IO(None)
            }
            cardIdOpt <- deckIdOpt match {
              case Some(deckId) => runTransaction(readCardIdByDeckIdAndContent(deckId, card))
                .handleErrorWith((ex: Throwable) =>
                  logger.error(ex)("Cannot find card by deck and content.") *> IO(None))
              case None => IO(None)
            }
            saveResult <- cardIdOpt match {
              case Some(cardId) =>
                runTransaction(AnswerInfoProgram.createAnswerInfo(info, cardId))
                  .handleErrorWith((ex: Throwable) =>
                    logger.error(ex)("Cannot save answer info.") *> IO(ServerError))
              case None => IO(ServerError)
            }
          } yield saveResult match {
            case ServerError => ServerErrorResponse
            case OperationSuccess => Response(Status.Created).withEntity(AnkiGenericResponse("Answer info is saved."))
            case _ => Response(Status.Accepted).withEntity(ErrorResponse("Unknown error. Answer info is not saved."))
          }
      executeValidated(request, createAnswerInfoInDatabase)
    }
    executeAuthenticated(request, cache, createInfo)
  }

  def improveAnswerInfo(request: Request[IO]): IO[Response[IO]] = {
    val improveInfo: String => IO[Response[IO]] = (userId: String) => {
      val createAnswerInfoInDatabase: (String, Card, AnswerInfo) => IO[Response[IO]] =
        (cardUUID: String, _: Card, info: AnswerInfo) =>
          for {
            cardIdOpt <- cache.get(userId).map(_.map(_.keyAliasMap).flatMap(_.get(cardUUID)))
            saveResult <- cardIdOpt match {
              case Some(cardId) => runTransaction(AnswerInfoProgram.createAnswerInfo(info, cardId))
                .handleErrorWith((ex: Throwable) =>
                  logger.error(ex)("Cannot save answer info.") *> IO(ServerError))
              case None => IO(ServerError)
            }
          } yield saveResult match {
            case ServerError => ServerErrorResponse
            case OperationSuccess => Response(Status.Created).withEntity(AnkiGenericResponse("Answer info is saved."))
            case _ => Response(Status.Accepted).withEntity(ErrorResponse("Unknown error. Answer info is not saved."))
          }
      executeValidated(request, createAnswerInfoInDatabase)
    }
    executeAuthenticated(request, cache, improveInfo)
  }

  def doCardsForImprove(request: Request[IO], limit: String): IO[Response[IO]] = {
    val takeIfNotEnough: (Int, Int, List[Int]) => IO[List[(Int, Card)]] =
      (given: Int, required: Int, fromDecks: List[Int]) =>
        if (given < required) runTransaction(
          readCardInfoWithSlowestSufficientAnswer(fromDecks, (required - given)))
          .handleErrorWith((ex: Throwable) =>
            logger.error(ex)("Cannot read card info with slowest answer.") *> IO(List())) else IO(List())

    if (limit.matches("^[0-9]*$")) {
      val selectCardsForImprove: String => IO[Response[IO]] = (userId: String) =>
        for {
          userNameOpt <- cache.get(userId).map(_.map(_.userName))
          deckIdList <- userNameOpt match {
            case Some(name) => runTransaction(readDeckIdListByUserName(name))
              .handleErrorWith((ex: Throwable) =>
                logger.error(ex)("Cannot receive decks by user.") *> IO(List()))
            case None => IO(List())
          }
          insufficientCardList <- runTransaction(
            readCardInfoListWithInsufficientAnswer(deckIdList, limit.toInt))
            .handleErrorWith((ex: Throwable) =>
              logger.error(ex)("Cannot read card infos with insufficient answer") *> IO(List()))
          sampleSize <- IO(insufficientCardList.size)
          sufficientCardList <- takeIfNotEnough(sampleSize, limit.toInt, deckIdList)
          resultList <- IO((insufficientCardList ++ sufficientCardList).distinct)
          resultSequentialIdList <- IO(resultList.map { case (id, _) => id })
          resultCardList <- IO(resultList.map { case (_, card) => card })
          resultAliasIdList <- IO(List.fill(resultList.length)(UUID.randomUUID().toString))
          keyAliasMap <- IO((resultAliasIdList zip resultSequentialIdList).toMap)
          aliasCardMap <- IO((resultAliasIdList zip resultCardList).toMap)
          sessionOpt <- cache.get(userId)
          result <- sessionOpt match {
            case Some(session) => cache.put(userId, UserSession(
              session.token,
              session.privileges,
              session.userName,
              session.loginAttempts,
              keyAliasMap)) *> IO(OperationSuccess)
            case None => IO(ServerError)
          }
        } yield result match {
          case OperationSuccess => if (aliasCardMap.nonEmpty)
            Response(Status.Ok).withEntity(CardsForImproveResponse(aliasCardMap)) else
            Response(Status.Accepted).withEntity(ErrorResponse("Cards for improve not found."))
          case ServerError => ServerErrorResponse
        }
      executeAuthenticated(request, cache, selectCardsForImprove)
    } else IO(Response(Status.Accepted).withEntity(ErrorResponse(s"Cannot parse cards size limit.")))
  }

  private def executeValidated(request: Request[IO], function: (String, Card, AnswerInfo) => IO[Response[IO]]):
  IO[Response[IO]] =
    request.as[CreateAnswerInfoRequest].redeemWith(
      error =>
        logger.error(error)("Cannot parse answer info request.") *>
          IO(Response(Status.Accepted).withEntity(ErrorResponse("Cannot parse 'answer info' request."))),
      answerInfoRequest => function.apply(answerInfoRequest.identity, answerInfoRequest.card, answerInfoRequest.info))
}