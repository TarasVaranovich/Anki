package edu.evolution.varanovich.anki.client

import cats.effect.{ExitCode, IO, IOApp}
import edu.evolution.varanovich.anki.client.AnkiClientCommand.{GenerateDeckCommand, _}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext

object AnkiClient extends IOApp {
  private implicit val cookies: UserCookies = new UserCookies()

  def process(client: Client[IO]): IO[Unit] = for {
    _ <- IO(println(
      s"""
         |Lets start Anki education
         |Choose menu option:
         |  N - generate new deck;
         |  L - get last generated deck;
         |  C - create new deck;
         |  F - find deck;
         |  U - get first(earliest) deck with unsolved cards;
         |  I - improve results of cards with weak answers;
         |  E - exit""".stripMargin))
    line <- IO(scala.io.StdIn.readLine())
    _ <- line match {
      case "N" => GenerateDeckCommand(client).run
      case "L" => LastGeneratedDeckCommand(client).run
      case "C" => CreateDeckCommand(client).run
      case "F" => FindDeckCommand(client).run
      case "U" => EarliestFreshDeckCommand(client).run
      case "I" => CardsForImproveCommand(client).run
      case "E" => IO.unit
      case _ => process(client)
    }
    _ <- IO.unit
  } yield ()

  private def welcome(client: Client[IO]): IO[Unit] =
    for {
      _ <- IO(println(
        s"""Register as a new user or login.
           |  R - register;
           |  L - login;
           |  E - exit""".stripMargin))
      line <- IO(scala.io.StdIn.readLine())
      _ <- line match {
        case "R" => RegisterCommand(client).run *> process(client)
        case "L" => LoginCommand(client).run *> process(client)
        case "E" => IO.unit
        case _ => welcome(client)
      }
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      for {
        _ <- IO(println(
          s"""
             |Welcome to anki application!""".stripMargin))
        _ <- welcome(client)
      } yield ()
    }.as(ExitCode.Success)
}