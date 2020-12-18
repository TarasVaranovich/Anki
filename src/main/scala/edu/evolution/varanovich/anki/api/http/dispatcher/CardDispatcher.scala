package edu.evolution.varanovich.anki.api.http.dispatcher

import cats.effect.{ContextShift, IO}
import doobie.implicits._
import edu.evolution.varanovich.anki.adt.{AnswerInfo, Card}
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode.{OperationSuccess, ServerError}
import edu.evolution.varanovich.anki.api.http.AnkiServer.ServerErrorResponse
import edu.evolution.varanovich.anki.api.http.dispatcher.DispatcherUtility.executeAuthenticated
import edu.evolution.varanovich.anki.api.http.protocol.AnkiRequest.CreateAnswerInfoRequest
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.{AnkiGenericResponse, ErrorResponse}
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AnswerInfoProgram
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.readCardIdByDeckIdAndContent
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.readDeckIdByDescriptionAndUserName
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

  private def executeValidated(request: Request[IO], function: (String, Card, AnswerInfo) => IO[Response[IO]]):
  IO[Response[IO]] =
    request.as[String].flatMap {
      body =>
        decode[CreateAnswerInfoRequest](body) match {
          case Left(_) =>
            IO(Response(Status.Accepted).withEntity(ErrorResponse("Cannot parse 'answer info' request.")))
          case Right(data) => function.apply(data.deckDescription, data.card, data.info)
        }
    }
}