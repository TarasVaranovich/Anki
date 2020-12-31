package edu.evolution.varanovich.anki.db

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie._
import doobie.implicits._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class DbManagerSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully connect to database" in {
    DbManager.transactor.use(0.pure[ConnectionIO].transact[IO]).map(result => assert(result == 0))
  }
}