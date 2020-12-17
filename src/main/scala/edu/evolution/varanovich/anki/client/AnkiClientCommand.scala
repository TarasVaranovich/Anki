package edu.evolution.varanovich.anki.client

import cats.effect.IO
import edu.evolution.varanovich.anki.adt.Card
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.{AnkiGenericResponse, DeckResponse, ErrorResponse, MultiErrorResponse, UserResponse}
import edu.evolution.varanovich.anki.client.AnkiClient.{cookies, process}
import edu.evolution.varanovich.anki.client.AnkiHttpRequest._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client

import scala.util.{Failure, Success, Try}

trait AnkiClientCommand {
  def run: IO[Unit]
}
object AnkiClientCommand {
  final case class RegisterCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] = for {
      _ <- IO(println(
        s"""Registration
           |Write user name in:""".stripMargin))
      name <- IO(scala.io.StdIn.readLine())
      _ <- IO(println("Write password in:"))
      password <- IO(scala.io.StdIn.readLine())
      response <- client.expect[AnkiResponse](RegisterRequest(name, password).send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
      _ <- response match {
        case UserResponse(id, token) => IO(cookies.updateCredentials(id, token)) *>
          IO(println("Registration successful.")) *> IO.unit
        case errorResponse => handleErrorResponse(errorResponse, RegisterCommand(client).run)
      }
    } yield ()
  }
  final case class LoginCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] = for {
      _ <- IO(println(
        s"""Login
           |Enter name:""".stripMargin))
      name <- IO(scala.io.StdIn.readLine())
      _ <- IO(println("Enter password:"))
      password <- IO(scala.io.StdIn.readLine())
      response <- client.expect[AnkiResponse](LoginRequest(name, password).send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse("Connection error")))
      _ <- response match {
        case UserResponse(id, token) => IO(cookies.updateCredentials(id, token)) *>
          IO(println("Login successful.")) *> IO.unit
        case errorResponse => handleErrorResponse(errorResponse, LoginCommand(client).run)
      }
    } yield ()
  }
  final case class CreateDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] = for {
      _ <- IO(println("Input deck name."))
      description <- IO(scala.io.StdIn.readLine())
      cardSet <- createCards(Set())
      response <- client.expect[AnkiResponse](CreateDeckRequest(description, cardSet.toArray).send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
      _ <- response match {
        case AnkiGenericResponse(message) => IO(println(message)) *> process(client)
        case errorResponse => handleErrorResponse(errorResponse, CreateDeckCommand(client).run)
      }
    } yield ()

    private def createCards(cards: Set[Card]): IO[Set[Card]] =
      for {
        _ <- IO(println("Input card question."))
        question <- IO(scala.io.StdIn.readLine())
        _ <- IO(println("Input card answer."))
        answer <- IO(scala.io.StdIn.readLine())
        _ <- IO(println("Type 'E' to end or any key to continue"))
        command <- IO(scala.io.StdIn.readLine())
        resultSet <- if (command.equals("E")) IO(cards ++ Set(Card(question, answer))) else
          createCards(cards ++ Set(Card(question, answer)))
      } yield resultSet
  }
  final case class FindDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        _ <- IO(println("Write string which deck start with."))
        pattern <- IO(scala.io.StdIn.readLine())
        response <- client.expect[AnkiResponse](LastDeckByPatternRequest(pattern).send)
          .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(println(
              s"""Last deck matching pattern received successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run)
          case errorResponse => handleErrorResponse(errorResponse, FindDeckCommand(client).run)
        }
      } yield ()
  }
  final case class GenerateDeckCommand(client: Client[IO])(implicit cookies: UserCookies) extends AnkiClientCommand {
    override def run: IO[Unit] =
      for {
        _ <- IO(println("Input deck size as number."))
        line <- IO(scala.io.StdIn.readLine())
        _ <- Try(line.toInt) match {
          case Success(size) => requestNewDeck(client, size)
          case Failure(_) => IO(println("Deck size should de integer.")) *> GenerateDeckCommand(client).run
        }
      } yield ()

    private def requestNewDeck(client: Client[IO], size: Int)(implicit cookies: UserCookies): IO[Unit] =
      for {
        response <- client.expect[AnkiResponse](GenerateDeckRequest(size).send)
          .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
        _ <- response match {
          case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
            IO(println(
              s"""Deck created successfully.
                 |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
            doCancellable(SolveDeckCommand(client, FirstCard).run)
          case errorResponse => handleErrorResponse(errorResponse, GenerateDeckCommand(client).run)
        }
      } yield ()
  }

  final case class LastGeneratedDeckCommand(client: Client[IO])(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] = for {
      response <- client.expect[AnkiResponse](LastDeckRequest().send)
        .handleErrorWith((_: Throwable) => IO(ErrorResponse(ErrorMessage)))
      _ <- response match {
        case DeckResponse(deck) => IO(cookies.updateDeck(deck)) *>
          IO(println(
            s"""Last generated deck received successfully.
               |Solve deck '${cookies.deck.description}'""".stripMargin)) *>
          doCancellable(SolveDeckCommand(client, FirstCard).run)
        case errorResponse => handleErrorResponse(errorResponse, LastGeneratedDeckCommand(client).run)
      }
    } yield ()
  }
  final case class SolveDeckCommand(client: Client[IO], cardNumber: Int)(implicit cookies: UserCookies)
    extends AnkiClientCommand {
    override def run: IO[Unit] = for {
      size <- IO(cookies.deck.cards.size)
      _ <- if (cardNumber > size)
        IO(println("Stub for results sum up.")) *> process(client) else
        for {
          _ <- IO(println(s"Question ${cardNumber}:"))
          _ <- IO(println(cookies.deck.cards.toList(cardNumber - FirstCard).question))
          _ <- IO(println(s"--Type 'N' for move to next card or 'E' for exit without save"))
          next <- IO(scala.io.StdIn.readLine())
          _ <- next match {
            case "N" => IO(println(cookies.deck.cards.toList(cardNumber - FirstCard).answer)) *>
              rateAnswer *> SolveDeckCommand(client, cardNumber + 1).run
            case "E" => IO.unit
            case _ => SolveDeckCommand(client, cardNumber).run
          }
        } yield ()
    } yield ()
    private def rateAnswer: IO[Unit] =
      for {
        _ <- IO(println(
          s"""Rate yourself:
             |  F - fail;
             |  H - hard;
             |  G - good;
             |  E - easy""".stripMargin))
        rate <- IO(scala.io.StdIn.readLine())
        _ <- rate match {
          case "F" => IO(println("Stubbed fail"))
          case "H" => IO(println("Stubbed hard"))
          case "G" => IO(println("Stubbed good"))
          case "E" => IO(println("Stubbed easy"))
          case _ => rateAnswer
        }
      } yield ()
  }

  private def doCancellable(operation: IO[Unit]): IO[Unit] =
    for {
      _ <- IO(println("Would you continue? Y - yes; N -no"))
      answer <- IO(scala.io.StdIn.readLine())
      _ <- answer match {
        case "Y" => operation
        case "N" => IO.unit
        case _ => doCancellable(operation)
      }
    } yield ()

  private def handleErrorResponse(response: AnkiResponse, callbackF: IO[Unit]): IO[Unit] = response match {
    case ErrorResponse(message) => IO(println(message)) *> doCancellable(callbackF)
    case MultiErrorResponse(messages) => IO(messages.foreach(println)) *> doCancellable(callbackF)
  }
}