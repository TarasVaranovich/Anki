package edu.evolution.varanovich.anki.api.http.dispatcher

import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxApply
import doobie.implicits._
import edu.evolution.varanovich.anki.adt.Deck
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode.ServerError
import edu.evolution.varanovich.anki.api.http.AnkiServer.ErrorResponse
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.createCardList
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.{createDeck, readDeckIdByDescriptionAndUserName}
import edu.evolution.varanovich.anki.db.program.entity.UserProgram.readSequentialId
import edu.evolution.varanovich.anki.domain.DeckBuilder
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxDeckLength, MinDeckLength}
import edu.evolution.varanovich.anki.utility.StringUtility.matches
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Header, Request, Response, Status}

import scala.util.{Failure, Success, Try}

object DeckDispatcher {
  private val unauthorizedResponse: Response[IO] =
    Response(Status.Unauthorized).withEntity(ErrorResponse(s"You are not logged in."))
  private final case class RandomDeckResponse(deck: Deck)
  def doRandom(size: String,
               request: Request[IO], cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    if (matches(size, "^[0-9]*$".r)) {
      val generateDeck: (Int) => (String) => IO[Response[IO]] = (sizeInt: Int) => (userId: String) =>
        for {
          deckOpt <- DeckBuilder.randomDeck(sizeInt)
          userNameOpt <- cache.get(userId).map(_.map(_.userName))
          userIdOpt <- userNameOpt match {
            case Some(name) => DbManager.transactor.use(readSequentialId(name).transact[IO])
              .handleErrorWith((_: Throwable) => IO(None))
            case None => IO(None)
          }
          deckIdOpt <- (userIdOpt, userNameOpt, deckOpt) match {
            case (Some(userId), Some(name), Some(deck)) =>
              DbManager.transactorBlock(
                createDeck(deck, userId) *> readDeckIdByDescriptionAndUserName(deck.description, name))
                .handleErrorWith((_: Throwable) => IO(None))
            case (_, _, _) => IO(None)
          }
          saveResult <- (deckIdOpt, deckOpt) match {
            case (Some(deckId), Some(deck)) =>
              DbManager.transactor.use(createCardList(deck.cards.toList, deckId).transact[IO])
                .handleErrorWith((_: Throwable) => IO(ServerError))
            case (_, _) => IO(ServerError)
          }
        } yield (deckOpt, saveResult) match {
          case (_, ServerError) => Response(Status.InternalServerError).withEntity(ErrorResponse(s"Cannot save deck."))
          case (Some(deck), _) => Response(Status.Created).withEntity(RandomDeckResponse(deck))
          case (None, _) => Response(Status.InternalServerError).withEntity(ErrorResponse(s"Cannot generate deck."))
        }
      Try(size.toInt) match {
        case Success(sizeInt) => if ((sizeInt >= MinDeckLength) && (sizeInt <= MaxDeckLength))
          executeAuthenticated(request, cache, generateDeck.apply(sizeInt)) else
          IO(Response(Status.NotAcceptable)
            .withEntity(ErrorResponse(s"Wrong deck size. Check if size in range $MinDeckLength..$MaxDeckLength.")))
        case Failure(_) =>
          IO(Response(Status.UnprocessableEntity).withEntity(ErrorResponse(s"Deck size string is to long.")))
      }

    } else IO(Response(Status.UnprocessableEntity).withEntity(ErrorResponse(s"Cannot parse deck size.")))
  }

  private def executeAuthenticated(request: Request[IO],
                                   cache: Cache[IO, String, UserSession],
                                   function: String => IO[Response[IO]]): IO[Response[IO]] = {
    val userIdHeaderOpt: Option[Header] = request.headers.get(CaseInsensitiveString.apply("user-id"))
    val tokenHeaderOpt: Option[Header] = request.headers.get(CaseInsensitiveString.apply("token"))
    (userIdHeaderOpt, tokenHeaderOpt) match {
      case (Some(userIdHeader), Some(tokenHeader)) =>
        for {
          tokenOpt <- cache.get(userIdHeader.value).map(_.map(_.token))
          response <- tokenOpt match {
            case Some(token) => if (token == tokenHeader.value) function.apply(userIdHeader.value)
            else IO(unauthorizedResponse)
            case None => IO(unauthorizedResponse)
          }
        } yield response
      case (_, _) => IO(unauthorizedResponse)
    }
  }
}