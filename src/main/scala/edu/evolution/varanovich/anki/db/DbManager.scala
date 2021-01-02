package edu.evolution.varanovich.anki.db

import cats.effect.{Blocker, ContextShift, IO, Resource}
import doobie.hikari.HikariTransactor
import doobie.implicits._
import doobie.{ConnectionIO, ExecutionContexts, Transactor}
import edu.evolution.varanovich.anki.config.DbConfig

object DbManager {
  private val config = DbConfig.load

  def transactor(implicit shift: ContextShift[IO]): Resource[IO, Transactor[IO]] =
    for {
      context <- ExecutionContexts.fixedThreadPool[IO](config.transactorPoolSize)
      blocker <- Blocker[IO]
      hikariTransactor <- HikariTransactor
        .newHikariTransactor[IO](config.dbDriver, config.dbUrl, config.dbUser, config.dbPassword, context, blocker)
    } yield hikariTransactor

  def transactorBlock[A](block: => ConnectionIO[A])(implicit shift: ContextShift[IO]): IO[A] =
    transactor.use(block.transact[IO])
}
