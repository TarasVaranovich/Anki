package edu.evolution.varanovich.anki.api.http.dispatcher

import java.util.UUID

import cats.effect.{ContextShift, IO}
import doobie.implicits._
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode.{OperationSuccess, ServerError}
import edu.evolution.varanovich.anki.api.http.AnkiServer.ServerErrorResponse
import edu.evolution.varanovich.anki.api.http.dispatcher.DispatcherUtility.executeAuthenticated
import edu.evolution.varanovich.anki.api.http.protocol.AnkiRequest.CreateAnswerInfoRequest
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.{AnkiGenericResponse, CardsForImproveResponse, ErrorResponse}
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AnswerInfoProgram
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.{readCardIdByDeckIdAndContent, readCardInfoListWithInsufficientAnswer, readCardInfoWithSlowestSufficientAnswer}
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.{readDeckIdByDescriptionAndUserName, readDeckIdListByUserName}
import edu.evolution.varanovich.anki.model.{AnswerInfo, Card}
import edu.evolution.varanovich.anki.utility.StringUtility.matches
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser.decode
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.{Request, Response, Status}

object CardDispatcher {
  def createAnswerInfo(request: Request[IO], cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    val createInfo: String => IO[Response[IO]] = (userId: String) => {
      val createAnswerInfoInDatabase: (String, Card, AnswerInfo) => IO[Response[IO]] =
        (description: String, card: Card, info: AnswerInfo) =>
          for {
            userNameOpt <- cache.get(userId).map(_.map(_.userName))
            deckIdOpt <- userNameOpt match {
              case Some(name) => DbManager.transactor.use(
                readDeckIdByDescriptionAndUserName(description, name).transact[IO])
                .handleErrorWith((_: Throwable) => IO(None))
              case None => IO(None)
            }
            cardIdOpt <- deckIdOpt match {
              case Some(deckId) =>
                DbManager.transactor.use(readCardIdByDeckIdAndContent(deckId, card).transact[IO])
                  .handleErrorWith((_: Throwable) => IO(None))
              case None => IO(None)
            }
            saveResult <- cardIdOpt match {
              case Some(cardId) =>
                DbManager.transactor.use(AnswerInfoProgram.createAnswerInfo(info, cardId).transact[IO])
                  .handleErrorWith((_: Throwable) => IO(ServerError))
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

  def improveAnswerInfo(request: Request[IO],
                        cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    val improveInfo: String => IO[Response[IO]] = (userId: String) => {
      val createAnswerInfoInDatabase: (String, Card, AnswerInfo) => IO[Response[IO]] =
        (cardUUID: String, _: Card, info: AnswerInfo) =>
          for {
            cardIdOpt <- cache.get(userId).map(_.map(_.keyAliasMap).flatMap(_.get(cardUUID)))
            saveResult <- cardIdOpt match {
              case Some(cardId) =>
                for {
                  result <- DbManager.transactor.use(AnswerInfoProgram.createAnswerInfo(info, cardId).transact[IO])
                    .handleErrorWith((_: Throwable) => IO(ServerError))
                } yield result
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

  def doCardsForImprove(request: Request[IO],
                        cache: Cache[IO, String, UserSession],
                        limit: String)(implicit contextShift: ContextShift[IO]): IO[Response[IO]] = {
    if (matches(limit, "^[0-9]*$".r)) {
      val selectCardsForImprove: String => IO[Response[IO]] = (userId: String) =>
        for {
          userNameOpt <- cache.get(userId).map(_.map(_.userName))
          deckIdList <- userNameOpt match {
            case Some(name) => DbManager.transactor.use(readDeckIdListByUserName(name).transact[IO])
              .handleErrorWith((_: Throwable) => IO(List()))
            case None => IO(List())
          }
          insufficientCardList <- DbManager.transactor.use(
            readCardInfoListWithInsufficientAnswer(deckIdList, limit.toInt).transact[IO])
            .handleErrorWith((_: Throwable) => IO(List()))
          sampleSize <- IO(insufficientCardList.size)
          sufficientCardList <- if (sampleSize < limit.toInt) DbManager.transactor.use(
            readCardInfoWithSlowestSufficientAnswer(deckIdList, (limit.toInt - sampleSize)).transact[IO])
            .handleErrorWith((_: Throwable) => IO(List())) else IO(List())
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
    request.as[String].flatMap {
      body =>
        decode[CreateAnswerInfoRequest](body) match {
          case Left(_) =>
            IO(Response(Status.Accepted).withEntity(ErrorResponse("Cannot parse 'answer info' request.")))
          case Right(data) => function.apply(data.identity, data.card, data.info)
        }
    }
}