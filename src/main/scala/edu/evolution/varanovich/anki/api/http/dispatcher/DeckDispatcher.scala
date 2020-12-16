package edu.evolution.varanovich.anki.api.http.dispatcher

import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxApply
import doobie.implicits._
import edu.evolution.varanovich.anki.adt.{Card, Deck}
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode._
import edu.evolution.varanovich.anki.api.http.AnkiServer.{AnkiResponse, ErrorResponse, MultiErrorResponse, ServerErrorResponse}
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.{createCardList, readCardList}
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram._
import edu.evolution.varanovich.anki.db.program.entity.UserProgram.readSequentialId
import edu.evolution.varanovich.anki.domain.DeckBuilder
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxDeckLength, MinDeckLength}
import edu.evolution.varanovich.anki.utility.StringUtility.matches
import edu.evolution.varanovich.anki.validator.DeckValidator
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser.decode
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Header, Request, Response, Status}

import scala.util.{Failure, Success, Try}

object DeckDispatcher {
  private val unauthorizedResponse: Response[IO] =
    Response(Status.Unauthorized).withEntity(ErrorResponse(s"You are not logged in."))
  private final case class DeckRequest(description: String, cards: Array[Card])
  private final case class DeckResponse(deck: Deck)
  def doRandom(size: String,
               request: Request[IO], cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    if (matches(size, "^[0-9]*$".r)) {
      val generateDeck: (Int) => (String) => IO[Response[IO]] = (sizeInt: Int) => (userId: String) =>
        for {
          deckOpt <- DeckBuilder.randomDeck(sizeInt)
          saveResult <- deckOpt match {
            case Some(deck) => saveDeckWithCards(deck, userId, cache)
            case None => IO(0)
          }
        } yield (deckOpt, saveResult) match {
          case (_, ServerError) => Response(Status.InternalServerError).withEntity(ErrorResponse(s"Cannot save deck."))
          case (Some(deck), _) => Response(Status.Created).withEntity(DeckResponse(deck))
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

  def doLastGenerated(request: Request[IO],
                      cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    val lastGeneratedDeck: String => IO[Response[IO]] = (userId: String) =>
      for {
        nameOpt <- cache.get(userId).map(_.map(_.userName))
        deckInfoOpt <- nameOpt match {
          case Some(name) => DbManager.transactor.use(readLastGeneratedDeckInfoByUserName(name).transact[IO])
            .handleErrorWith((_: Throwable) => IO(None))
          case None => IO(None)
        }
        cardList <- deckInfoOpt match {
          case Some((id, _)) => DbManager.transactor.use(readCardList(id).transact[IO])
            .handleErrorWith((_: Throwable) => IO(List()))
          case None => IO(List())
        }
        description <- IO(deckInfoOpt.map { case (_, description) => description }.getOrElse(""))
      } yield Deck.from(cardList.toSet, description) match {
        case Some(deck) => Response(Status.Ok).withEntity(DeckResponse(deck))
        case None => Response(Status.NotFound).withEntity(ErrorResponse(s"Deck not found."))
      }
    executeAuthenticated(request, cache, lastGeneratedDeck)
  }

  def doSave(request: Request[IO],
             cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]):
  IO[Response[IO]] = {
    val saveDeck: String => IO[Response[IO]] = (userId: String) => {
      val saveToDatabase: Deck => IO[Response[IO]] = (deck: Deck) => for {
        saveResult <- saveDeckWithCards(deck, userId, cache)
      } yield saveResult match {
        case ServerError => ServerErrorResponse
        case value => if (value == deck.cards.size) Response(Status.Ok).withEntity(AnkiResponse("Deck is saved.")) else
          Response(Status.InternalServerError).withEntity(ErrorResponse("Unknown error. Deck not saved."))
      }
      executeValidated(request, saveToDatabase)
    }
    executeAuthenticated(request, cache, saveDeck)
  }

  private def saveDeckWithCards(deck: Deck,
                                userId: String,
                                cache: Cache[IO, String, UserSession])(implicit contextShift: ContextShift[IO]): IO[Int] =
    for {
      userNameOpt <- cache.get(userId).map(_.map(_.userName))
      userIdOpt <- userNameOpt match {
        case Some(name) => DbManager.transactor.use(readSequentialId(name).transact[IO])
          .handleErrorWith((_: Throwable) => IO(None))
        case None => IO(None)
      }
      deckIdOpt <- (userIdOpt, userNameOpt) match {
        case (Some(userId), Some(name)) =>
          DbManager.transactorBlock(
            createDeck(deck, userId) *> readDeckIdByDescriptionAndUserName(deck.description, name))
            .handleErrorWith((_: Throwable) => IO(None))
        case (_, _) => IO(None)
      }
      saveResult <- deckIdOpt match {
        case Some(deckId) =>
          DbManager.transactor.use(createCardList(deck.cards.toList, deckId).transact[IO])
            .handleErrorWith((_: Throwable) => IO(ServerError))
        case None => IO(ServerError)
      }
    } yield saveResult

  private def executeValidated(request: Request[IO], function: Deck => IO[Response[IO]]): IO[Response[IO]] =
    request.as[String].flatMap {
      body =>
        decode[DeckRequest](body) match {
          case Left(_) =>
            IO(Response(Status.UnprocessableEntity).withEntity(ErrorResponse("Cannot parse deck from request body.")))
          case Right(deckData) => {
            DeckValidator.validate(deckData.cards.toList, deckData.description).toEither match {
              case Left(errors) => IO(Response(Status.NotAcceptable)
                .withEntity(MultiErrorResponse(errors.map(_.message).toNonEmptyList.toList.toArray)))
              case Right(deck) => function.apply(deck)
            }
          }
        }
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