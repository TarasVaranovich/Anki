package edu.evolution.varanovich.anki.api.http.dispatcher

import cats.effect.{IO, Resource}
import cats.implicits.catsSyntaxApply
import doobie.Transactor
import doobie.implicits._
import edu.evolution.varanovich.anki.api.http.AnkiErrorCode._
import edu.evolution.varanovich.anki.api.http.AnkiServer.ServerErrorResponse
import edu.evolution.varanovich.anki.api.http.dispatcher.DispatcherUtility.executeAuthenticated
import edu.evolution.varanovich.anki.api.http.protocol.AnkiRequest._
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse._
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.domain.CardProgram.{createCardList, readCardList}
import edu.evolution.varanovich.anki.db.program.domain.DeckProgram._
import edu.evolution.varanovich.anki.db.program.domain.UserProgram.readSequentialId
import edu.evolution.varanovich.anki.domain.DeckBuilder
import edu.evolution.varanovich.anki.model.Deck
import edu.evolution.varanovich.anki.validator.DeckValidator
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.{Request, Response, Status}
import org.http4s.circe.CirceEntityCodec.{circeEntityEncoder, _}

import scala.util.{Failure, Success, Try}

final case class DeckDispatcher(logger: SelfAwareStructuredLogger[IO], config: AnkiConfig)(
  implicit transactor: Resource[IO, Transactor[IO]], cache: Cache[IO, String, UserSession]) {
  private val deckBuilder = DeckBuilder(config)

  def doRandom(size: String, request: Request[IO]): IO[Response[IO]] = {
    if (size.matches("^[0-9]*$")) {
      val generateDeck: Int => String => IO[Response[IO]] = (sizeInt: Int) => (userId: String) =>
        for {
          deckOpt <- deckBuilder.randomDeck(sizeInt)
          saveResult <- deckOpt match {
            case Some(deck) => saveDeckWithCards(deck, userId)
            case None => IO(0)
          }
        } yield (deckOpt, saveResult) match {
          case (_, ServerError) => Response(Status.Accepted).withEntity(ErrorResponse(s"Cannot save deck."))
          case (Some(deck), _) => Response(Status.Created).withEntity(DeckResponse(deck))
          case (None, _) => Response(Status.Accepted).withEntity(ErrorResponse(s"Cannot generate deck."))
        }

      val generateValidated: Int => IO[Response[IO]] = (size: Int) =>
        if ((size >= config.minDeckLength) && (size <= config.maxDeckLength))
          executeAuthenticated(request, cache, generateDeck.apply(size)) else
          IO(Response(Status.Accepted).withEntity(ErrorResponse(
            s"Wrong deck size. Check if size in range ${config.minDeckLength}..${config.maxDeckLength}.")))

      Try(size.toInt) match {
        case Success(sizeInt) => generateValidated(sizeInt)
        case Failure(failure) => logger.error(failure)("Cannot read deck size.")
          IO(Response(Status.Accepted).withEntity(ErrorResponse(s"Deck size string is to long.")))
      }

    } else IO(Response(Status.Accepted).withEntity(ErrorResponse(s"Cannot parse deck size.")))
  }

  def doLastGenerated(request: Request[IO]): IO[Response[IO]] = {
    val lastGeneratedDeck: String => IO[Response[IO]] = (userId: String) =>
      readDeckWithCards(config.generatedDeckName, userId).map {
        case Some(deck) => Response(Status.Ok).withEntity(DeckResponse(deck))
        case None => Response(Status.Accepted).withEntity(ErrorResponse(s"Deck not found."))
      }
    executeAuthenticated(request, cache, lastGeneratedDeck)
  }

  def doSave(request: Request[IO]): IO[Response[IO]] = {
    val saveDeck: String => IO[Response[IO]] = (userId: String) => {
      val saveToDatabase: Deck => IO[Response[IO]] = (deck: Deck) =>
        saveDeckWithCards(deck, userId).map {
          case ServerError => ServerErrorResponse
          case value => if (value == deck.cards.size)
            Response(Status.Created).withEntity(AnkiGenericResponse("Deck is saved.")) else
            Response(Status.Accepted).withEntity(ErrorResponse("Unknown error. Deck not saved."))
        }
      executeValidated(request, saveToDatabase)
    }
    executeAuthenticated(request, cache, saveDeck)
  }

  def doLastByPattern(request: Request[IO]): IO[Response[IO]] = {
    val lastGeneratedDeck: String => IO[Response[IO]] = (userId: String) =>
      request.as[AnkiGenericRequest].redeemWith(
        error => logger.error(error)("Cannot parse anki generic request.") *> IO(None),
        ankiRequest => readDeckWithCards(ankiRequest.data, userId))
        .flatMap {
          case Some(deck) => IO(Response(Status.Ok).withEntity(DeckResponse(deck)))
          case None => IO(Response(Status.Accepted).withEntity(ErrorResponse(s"Deck not found.")))
        }
    executeAuthenticated(request, cache, lastGeneratedDeck)
  }

  def doEarliestFresh(request: Request[IO]): IO[Response[IO]] = {
    val lastGeneratedDeck: String => IO[Response[IO]] = (userId: String) =>
      for {
        userNameOpt <- cache.get(userId).map(_.map(_.userName))
        deckInfoOpt <- userNameOpt match {
          case Some(name) => runTransaction(readEarliestFreshDeckInfo(name)).handleErrorWith((ex: Throwable) =>
            logger.error(ex)("Cannot read earliest fresh deck info.") *> IO(None))
          case None => IO(None)
        }
        cardList <- deckInfoOpt match {
          case Some((id, _)) => runTransaction(readCardList(id)).handleErrorWith((ex: Throwable) =>
            logger.error(ex)("Cannot receive card list by deck.") *> IO(List()))
          case None => IO(List())
        }
        description <- IO(deckInfoOpt.map { case (_, description) => description }.getOrElse(""))
      } yield Deck.from(cardList.toSet, description) match {
        case Some(deck) => Response(Status.Ok).withEntity(DeckResponse(deck))
        case None => Response(Status.Accepted).withEntity(ErrorResponse(s"Deck not found."))
      }
    executeAuthenticated(request, cache, lastGeneratedDeck)
  }

  private def saveDeckWithCards(deck: Deck, userId: String): IO[Int] =
    for {
      userNameOpt <- cache.get(userId).map(_.map(_.userName))
      userIdOpt <- userNameOpt match {
        case Some(name) => runTransaction(readSequentialId(name)).handleErrorWith((ex: Throwable) =>
          logger.error(ex)("Cannot read user identifier.") *> IO(None))
        case None => IO(None)
      }
      deckIdOpt <- (userIdOpt, userNameOpt) match {
        case (Some(userId), Some(name)) =>
          runTransaction(createDeck(deck, userId) *> readDeckIdByDescriptionAndUserName(deck.description, name))
            .handleErrorWith((ex: Throwable) =>
              logger.error(ex)("Cannot read deck by by description and user.") *> IO(None))
        case (_, _) => IO(None)
      }
      saveResult <- deckIdOpt match {
        case Some(deckId) =>
          DbManager.runTransaction(createCardList(deck.cards.toList, deckId)).handleErrorWith((ex: Throwable) =>
            logger.error(ex)("Cannot save card list.") *> IO(ServerError))
        case None => IO(ServerError)
      }
    } yield saveResult

  private def readDeckWithCards(pattern: String, userId: String): IO[Option[Deck]] =
    for {
      nameOpt <- cache.get(userId).map(_.map(_.userName))
      deckInfoOpt <- nameOpt match {
        case Some(name) => runTransaction(readLastDeckInfoByPatternAndUserName(pattern, name))
          .handleErrorWith((ex: Throwable) =>
            logger.error(ex)("Cannot find last deck info by pattern and user name.") *> IO(None))
        case None => IO(None)
      }
      cardList <- deckInfoOpt match {
        case Some((id, _)) => runTransaction(readCardList(id)).handleErrorWith((ex: Throwable) =>
          logger.error(ex)("Cannot receive deck card list.") *> IO(List()))
        case None => IO(List())
      }
      description <- IO(deckInfoOpt.map { case (_, description) => description }.getOrElse(""))
    } yield Deck.from(cardList.toSet, description)

  private def executeValidated(request: Request[IO], function: Deck => IO[Response[IO]]): IO[Response[IO]] =
    request.as[DeckRequest].redeemWith(
      error =>
        logger.error(error)("Cannot parse deck request.") *>
          IO(Response(Status.Accepted).withEntity(ErrorResponse("Cannot parse deck from request body."))),
      deckRequest =>
        DeckValidator.validate(deckRequest.cards.toList, deckRequest.description).toEither match {
          case Left(errors) => IO(Response(Status.Accepted)
            .withEntity(MultiErrorResponse(errors.map(_.message).toNonEmptyList.toList.toArray)))
          case Right(deck) => function.apply(deck)
        })
}