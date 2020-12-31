package edu.evolution.varanovich.anki.client

import AnkiClient.{process, welcome}
import cats.effect.IO
import cats.effect.IO.ioEffect
import edu.evolution.varanovich.anki.model.Rate.{Easy, Fail, Good, Hard}
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse._
import edu.evolution.varanovich.anki.client.AnkiHttpRequest._
import edu.evolution.varanovich.anki.model.{AnswerInfo, Card, Rate}
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client

import scala.util.{Failure, Success, Try}

trait AnkiClientCommand {
  def run: IO[Unit]
}
object AnkiClientCommand {
  final case class RegisterCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        name <- IO(println(
          s"""Registration
             |Write user name in:""".stripMargin)) *> IO(scala.io.StdIn.readLine())
        password <- IO(println("Write password in:")) *> IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](RegisterRequest(name, password).send)
          .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case UserResponse(id, token) => IO(cookies.updateCredentials(id, token)) *>
            IO(println("Registration successful.")) *> process(client)
          case errorResponse => handleErrorResponse(errorResponse, RegisterCommand(client).run, client, welcome)
        }
      } yield ()
  }
  final case class LoginCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        name <- IO(println(
          s"""Login
             |Enter name:""".stripMargin)) *> IO(scala.io.StdIn.readLine())
        password <- IO(println("Enter password:")) *> IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](LoginRequest(name, password).send)
          .handleErrorWith((_: Throwable) => IO(ErrorResponse("Connection error")))
        _ <- response match {
          case UserResponse(id, token) => IO(cookies.updateCredentials(id, token)) *>
            IO(println("Login successful.")) *> process(client)
          case errorResponse => handleErrorResponse(errorResponse, LoginCommand(client).run, client, welcome)
        }
      } yield ()
  }
  final case class CreateDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        description <- IO(println("Input deck name.")) *> IO(scala.io.StdIn.readLine())
        cardSet <- createCards(Set())
        response <- client.expect[AnkiResponse](CreateDeckRequest(description, cardSet.toArray).send)
          .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case AnkiGenericResponse(message) => IO(println(message)) *> process(client)
          case errorResponse => handleErrorResponse(errorResponse, CreateDeckCommand(client).run, client)
        }
      } yield ()

    private def createCards(cards: Set[Card]): IO[Set[Card]] =
      for {
        question <- IO(println("Input card question.")) *> IO(scala.io.StdIn.readLine())
        answer <- IO(println("Input card answer.")) *> IO(scala.io.StdIn.readLine())
        command <- IO(println("Type 'E' to end or any key to continue")) *> IO(scala.io.StdIn.readLine())
        resultSet <- if (command.equals("E")) IO(cards ++ Set(Card(question, answer))) else
          createCards(cards ++ Set(Card(question, answer)))
      } yield resultSet
  }
  final case class FindDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        pattern <- IO(println("Write string which deck start with.")) *> IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](LastDeckByPatternRequest(pattern).send)
          .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(println(
              s"""Last deck matching pattern received successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, FindDeckCommand(client).run, client)
        }
      } yield ()
  }
  final case class GenerateDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      IO(println("Input deck size as number.")) *>
        IO(scala.io.StdIn.readLine()).map(line => Try(line.toInt))
          .flatMap {
            case Success(size) => requestNewDeck(client, size)
            case Failure(_) => IO(println("Deck size should de integer.")) *> GenerateDeckCommand(client).run
          }

    private def requestNewDeck(client: Client[IO], size: Int)(implicit cookies: UserCookies): IO[Unit] =
      client.expect[AnkiResponse](GenerateDeckRequest(size).send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(println(
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
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(println(
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
          if (cardNumber > size) IO(println("Deck solving completed.")) *> process(client) else continueSolving)

    private def continueSolving: IO[Unit] =
      for {
        millis <- IO(println(
          s"""
             |Question ${cardNumber}:""".stripMargin)) *> IO(System.currentTimeMillis())
        next <- IO(cookies.updateTimeStamp(millis)) *>
          IO(println(cookies.cardList(cardNumber - FirstCard).question)) *>
          IO(println(s"--Type 'N' for move to next card or 'E' for exit without save")) *>
          IO(scala.io.StdIn.readLine())
        _ <- next match {
          case "N" => IO(cookies.calculateDurationSec(System.currentTimeMillis())) *>
            IO(println(
              s"""
                 |${cookies.cardList(cardNumber - FirstCard).answer}""".stripMargin)) *>
            rateAnswer(cookies.cardList(cardNumber - FirstCard), client) *>
            SolveDeckCommand(client, cardNumber + 1).run
          case "E" => IO.unit
          case _ => SolveDeckCommand(client, cardNumber).run
        }
      } yield ()

    private def rateAnswer(card: Card, client: Client[IO])(implicit cookies: UserCookies): IO[Unit] =
      IO(println(
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
              .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
            _ <- response match {
              case AnkiGenericResponse(message) => IO(println(message))
              case errorResponse => handleErrorResponse(errorResponse, rateAnswer(card, client), client)
            }
          } yield ()
          case None => IO(println("Client application error")) *> process(client)
        }
      } yield ()
  }

  final case class EarliestFreshDeckCommand(client: Client[IO])(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] =
      client.expect[AnkiResponse](EarliestFreshDeckRequest().send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(println(
              s"""Earliest deck with unsolved cards received successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, EarliestFreshDeckCommand(client).run, client)
        }
  }

  final case class CardsForImproveCommand(client: Client[IO])(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] =
      IO(println("Input size limit of card set as number.")) *>
        IO(scala.io.StdIn.readLine())
          .map(line => Try(line.toInt))
          .flatMap {
            case Success(size) => requestCardsForImprove(client, size)
            case Failure(_) => IO(println("Deck size should de integer.")) *> GenerateDeckCommand(client).run
          }

    private def requestCardsForImprove(client: Client[IO], limit: Int)(implicit cookies: UserCookies): IO[Unit] =
      client.expect[AnkiResponse](CardsForImproveRequest(limit).send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        .flatMap {
          case CardsForImproveResponse(cardsMap) => IO(cookies.updateTemporaryDeck(cardsMap)) *>
            IO(println(
              s"""Generated temporary set from cards which results should be improved.
                 |Available ${cardsMap.size} of announced limit $limit.""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run, client)
          case errorResponse => handleErrorResponse(errorResponse, CardsForImproveCommand(client).run, client)
        }
  }

  private def doCancellable(operation: IO[Unit],
                            client: Client[IO],
                            rejectF: Client[IO] => IO[Unit] = process): IO[Unit] =
    IO(println("Would you continue? Y - yes; N - no")) *>
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
    case ErrorResponse(message) => IO(println(message)) *> doCancellable(callbackF, client, rejectF)
    case MultiErrorResponse(messages) => IO(messages.foreach(println)) *> doCancellable(callbackF, client, rejectF)
  }
}