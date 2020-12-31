package edu.evolution.varanovich.anki.client

import AnkiClient.{printAdjustable, process, welcome}
import cats.effect.{IO, Sync}
import cats.effect.IO.ioEffect
import edu.evolution.varanovich.anki.model.Rate.{Easy, Fail, Good, Hard}
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse._
import edu.evolution.varanovich.anki.client.AnkiHttpRequest._
import edu.evolution.varanovich.anki.model.{AnswerInfo, Card, Rate}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client

import scala.util.{Failure, Success, Try}

trait AnkiClientCommand {
  def run: IO[Unit]
}
object AnkiClientCommand {
  private implicit def logger[F[_] : Sync] = Slf4jLogger.getLogger[F]

  final case class RegisterCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        name <- IO(printAdjustable(
          s"""Registration
             |Write user name in:""".stripMargin)) *> IO(scala.io.StdIn.readLine())
        password <- IO(printAdjustable("Write password in:")) *> IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](RegisterRequest(name, password).send)
          .handleErrorWith((ex: Throwable) =>
            logger[IO].error(ex)("Cannot register.") *> IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case UserResponse(id, token) => IO(cookies.updateCredentials(id, token)) *>
            IO(printAdjustable("Registration successful.")) *> process(client)
          case errorResponse => handleErrorResponse(errorResponse, RegisterCommand(client).run, client, welcome)
        }
      } yield ()
  }
  final case class LoginCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        name <- IO(printAdjustable(
          s"""Login
             |Enter name:""".stripMargin)) *> IO(scala.io.StdIn.readLine())
        password <- IO(printAdjustable("Enter password:")) *> IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](LoginRequest(name, password).send)
          .handleErrorWith((ex: Throwable) =>
            logger[IO].error(ex)("Cannot login.") *> IO(ErrorResponse("Connection error")))
        _ <- response match {
          case UserResponse(id, token) => IO(cookies.updateCredentials(id, token)) *>
            IO(printAdjustable("Login successful.")) *> process(client)
          case errorResponse => handleErrorResponse(errorResponse, LoginCommand(client).run, client, welcome)
        }
      } yield ()
  }
  final case class CreateDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        description <- IO(printAdjustable("Input deck name.")) *> IO(scala.io.StdIn.readLine())
        cardSet <- createCards(Set())
        response <- client.expect[AnkiResponse](CreateDeckRequest(description, cardSet.toArray).send)
          .handleErrorWith((ex: Throwable) =>
            logger[IO].error(ex)("Cannot create deck.") *> IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case AnkiGenericResponse(message) => IO(printAdjustable(message)) *> process(client)
          case errorResponse => handleErrorResponse(errorResponse, CreateDeckCommand(client).run, client)
        }
      } yield ()

    private def createCards(cards: Set[Card]): IO[Set[Card]] =
      for {
        question <- IO(printAdjustable("Input card question.")) *> IO(scala.io.StdIn.readLine())
        answer <- IO(printAdjustable("Input card answer.")) *> IO(scala.io.StdIn.readLine())
        command <- IO(printAdjustable("Type 'E' to end or any key to continue")) *> IO(scala.io.StdIn.readLine())
        resultSet <- if (command.equals("E")) IO(cards ++ Set(Card(question, answer))) else
          createCards(cards ++ Set(Card(question, answer)))
      } yield resultSet
  }
  final case class FindDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        pattern <- IO(printAdjustable("Write string which deck start with.")) *> IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](LastDeckByPatternRequest(pattern).send)
          .handleErrorWith((ex: Throwable) =>
            logger[IO].error(ex)("Cannot receive deck.") *> IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(printAdjustable(
              s"""Last deck matching pattern received successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, FindDeckCommand(client).run, client)
        }
      } yield ()
  }
  final case class GenerateDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      IO(printAdjustable("Input deck size as number.")) *>
        IO(scala.io.StdIn.readLine()).map(line => Try(line.toInt))
          .flatMap {
            case Success(size) => requestNewDeck(client, size)
            case Failure(_) => IO(printAdjustable("Deck size should de integer.")) *>
              GenerateDeckCommand(client).run
          }

    private def requestNewDeck(client: Client[IO], size: Int)(implicit cookies: UserCookies): IO[Unit] =
      client.expect[AnkiResponse](GenerateDeckRequest(size).send)
        .handleErrorWith((ex: Throwable) =>
          logger[IO].error(ex)("Cannot generate deck.") *> IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(printAdjustable(
              s"""Deck created successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, GenerateDeckCommand(client).run, client)
        }
  }

  final case class LastGeneratedDeckCommand(client: Client[IO])(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] =
      client.expect[AnkiResponse](LastDeckRequest().send)
        .handleErrorWith((ex: Throwable) =>
          logger[IO].error(ex)("Cannot receive last deck.") *> IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(printAdjustable(
              s"""Last generated deck received successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, LastGeneratedDeckCommand(client).run, client)
        }
  }
  final case class SolveDeckCommand(client: Client[IO], cardNumber: Int)(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] =
      IO(cookies.cardList.size)
        .flatMap(size =>
          if (cardNumber > size) IO(printAdjustable("Deck solving completed.")) *> process(client) else continueSolving)

    private def continueSolving: IO[Unit] =
      for {
        millis <- IO(printAdjustable(
          s"""
             |Question ${cardNumber}:""".stripMargin)) *> IO(System.currentTimeMillis())
        next <- IO(cookies.updateTimeStamp(millis)) *>
          IO(printAdjustable(cookies.cardList(cardNumber - FirstCard).question)) *>
          IO(printAdjustable(s"--Type 'N' for move to next card or 'E' for exit without save")) *>
          IO(scala.io.StdIn.readLine())
        _ <- next match {
          case "N" => IO(cookies.calculateDurationSec(System.currentTimeMillis())) *>
            IO(printAdjustable(
              s"""
                 |${cookies.cardList(cardNumber - FirstCard).answer}""".stripMargin)) *>
            rateAnswer(cookies.cardList(cardNumber - FirstCard), client) *>
            SolveDeckCommand(client, cardNumber + 1).run
          case "E" => IO.unit
          case _ => SolveDeckCommand(client, cardNumber).run
        }
      } yield ()

    private def rateAnswer(card: Card, client: Client[IO])(implicit cookies: UserCookies): IO[Unit] =
      IO(printAdjustable(
        s"""Rate yourself:
           |  F - fail;
           |  H - hard;
           |  G - good;
           |  E - easy""".stripMargin)) *>
        IO(scala.io.StdIn.readLine()).flatMap {
          case "F" => saveRate(Fail, card, client)
          case "H" => saveRate(Hard, card, client)
          case "G" => saveRate(Good, card, client)
          case "E" => saveRate(Easy, card, client)
          case _ => rateAnswer(card, client)
        }

    private def saveRate(rate: Rate, card: Card, client: Client[IO])(implicit cookies: UserCookies): IO[Unit] =
      for {
        identity <- if (cookies.isTemporaryCardSet) IO(cookies.resolveCardIdentity(card)) else
          IO(cookies.deck.description)
        answerInfoOpt <- IO(AnswerInfo.from(rate, cookies.lastAnswerDurationSec))
        _ <- answerInfoOpt match {
          case Some(info) => for {
            response <- client.expect[AnkiResponse](
              SaveAnswerInfoRequest(identity, card, info, cookies.isTemporaryCardSet).send)
              .handleErrorWith((ex: Throwable) =>
                logger[IO].error(ex)("Cannot save answer info.") *> IO(ErrorResponse(ErrorMessage)))
            _ <- response match {
              case AnkiGenericResponse(message) => IO(printAdjustable(message))
              case errorResponse => handleErrorResponse(errorResponse, rateAnswer(card, client), client)
            }
          } yield ()
          case None => IO(printAdjustable("Client application error")) *> process(client)
        }
      } yield ()
  }

  final case class EarliestFreshDeckCommand(client: Client[IO])(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] =
      client.expect[AnkiResponse](EarliestFreshDeckRequest().send)
        .handleErrorWith((ex: Throwable) =>
          logger[IO].error(ex)("Cannot receive deck.") *> IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(printAdjustable(
              s"""Earliest deck with unsolved cards received successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, EarliestFreshDeckCommand(client).run, client)
        }
  }

  final case class CardsForImproveCommand(client: Client[IO])(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] =
      IO(printAdjustable("Input size limit of card set as number.")) *>
        IO(scala.io.StdIn.readLine())
          .map(line => Try(line.toInt))
          .flatMap {
            case Success(size) => requestCardsForImprove(client, size)
            case Failure(_) => IO(printAdjustable("Deck size should de integer.")) *>
              GenerateDeckCommand(client).run
          }

    private def requestCardsForImprove(client: Client[IO], limit: Int)(implicit cookies: UserCookies): IO[Unit] =
      client.expect[AnkiResponse](CardsForImproveRequest(limit).send)
        .handleErrorWith((ex: Throwable) =>
          logger[IO].error(ex)("Cannot receive cards.") *> IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case CardsForImproveResponse(cardsMap) => IO(cookies.updateTemporaryDeck(cardsMap)) *>
            IO(printAdjustable(
              s"""Generated temporary set from cards which results should be improved.
                 |Available ${cardsMap.size} of announced limit $limit.""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, CardsForImproveCommand(client).run, client)
        }
  }

  private def doCancellable(operation: IO[Unit],
                            client: Client[IO],
                            rejectF: Client[IO] => IO[Unit] = process): IO[Unit] =
    IO(printAdjustable("Would you continue? Y - yes; N - no")) *>
      IO(scala.io.StdIn.readLine())
        .flatMap {
          case "Y" => operation
          case "N" => rejectF(client)
          case _ => doCancellable(operation, client)
        }

  private def handleErrorResponse(response: AnkiResponse,
                                  callbackF: IO[Unit],
                                  client: Client[IO],
                                  rejectF: Client[IO] => IO[Unit] = process):
  IO[Unit] = response match {
    case ErrorResponse(message) => IO(printAdjustable(message)) *> doCancellable(callbackF, client, rejectF)
    case MultiErrorResponse(messages) => IO(messages.foreach(printAdjustable)) *>
      doCancellable(callbackF, client, rejectF)
  }
}