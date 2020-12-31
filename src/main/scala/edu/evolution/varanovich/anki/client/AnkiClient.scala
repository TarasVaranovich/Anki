package edu.evolution.varanovich.anki.client

import cats.effect.{ExitCode, IO, IOApp}
import edu.evolution.varanovich.anki.client.AnkiClientCommand._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext

object AnkiClient extends IOApp {
  private implicit val cookies: UserCookies = new UserCookies()

  def process(client: Client[IO]): IO[Unit] =
    IO(println(
      s"""
         |Lets start Anki education
         |Choose menu option:
         |  N - generate new deck;
         |  L - get last generated deck;
         |  C - create new deck;
         |  F - find deck;
         |  U - get first(earliest) deck with unsolved cards;
         |  I - improve results of cards with weak answers;
         |  E - exit""".stripMargin)) *>
      IO(scala.io.StdIn.readLine()).flatMap {
        case "N" => GenerateDeckCommand(client).run
        case "L" => LastGeneratedDeckCommand(client).run
        case "C" => CreateDeckCommand(client).run
        case "F" => FindDeckCommand(client).run
        case "U" => EarliestFreshDeckCommand(client).run
        case "I" => CardsForImproveCommand(client).run
        case "E" => welcome(client)
        case _ => process(client)
      }

  def welcome(client: Client[IO]): IO[Unit] =
    IO(println(
      s"""Register as a new user or login.
         |  R - register;
         |  L - login;
         |  E - exit""".stripMargin)) *>
      IO(scala.io.StdIn.readLine()).flatMap {
        case "R" => RegisterCommand(client).run
        case "L" => LoginCommand(client).run
        case "E" => IO.unit
        case _ => welcome(client)
      }

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      IO(println(
        s"""
           |Welcome to anki application!""".stripMargin)) *>
        welcome(client)
    }.as(ExitCode.Success)
}