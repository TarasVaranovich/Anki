package edu.evolution.varanovich.anki.db

import cats.effect.{Blocker, ContextShift, IO, Resource}
import doobie.hikari.HikariTransactor
import doobie.implicits._
import doobie.{ConnectionIO, ExecutionContexts, Transactor}
import edu.evolution.varanovich.anki.config.DbConfig

object DbManager {
  private val config = DbConfig.load

  def transactorInstance(implicit shift: ContextShift[IO]): Resource[IO, Transactor[IO]] =
    for {
      context <- ExecutionContexts.fixedThreadPool[IO](config.transactorPoolSize)
      blocker <- Blocker[IO]
      hikariTransactor <- HikariTransactor
        .newHikariTransactor[IO](config.dbDriver, config.dbUrl, config.dbUser, config.dbPassword, context, blocker)
    } yield hikariTransactor

  def runTransaction[A](queries: => ConnectionIO[A])(implicit currentTransactor: Resource[IO, Transactor[IO]]): IO[A] =
    currentTransactor.use(queries.transact[IO])
}
