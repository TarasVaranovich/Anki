package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class AdjectiveProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      adjective <- IO.fromOption(highAdjectiveOpt)(throw new Exception("Adjective not created."))
      insertResult <- runTransaction(createAdjectiveTable *> createAdjective(adjective))
      adjectiveModified <- IO.fromOption(highAdjectiveModifiedOpt)(throw new Exception("Modified adjective not created"))
      updateResult <- runTransaction(updateAdjective(adjectiveModified))
      resultOpt <- runTransaction(readAdjective(adjective.value))
      result <- IO.fromOption(resultOpt)(throw new Exception("Adjective not found"))
      deleteResult <- runTransaction(deleteAdjective(adjective))
      deleted <- runTransaction(readAdjective(adjective.value))
      dropResult <- runTransaction(dropTable("adjective"))
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.translation == adjectiveModified.translation)
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropResult == 0)
    }
  }

  "should successfully perform list operations" in {
    for {
      list <- IO(List(bigAdjectiveOpt, clumsyAdjectiveOpt, highAdjectiveOpt).sequence.getOrElse(List()))
      createResult <- runTransaction(createAdjectiveTable *> createAdjectiveListSafely(list))
      listResult <- runTransaction(readAllAdjectives)
      dropTableResult <- runTransaction(dropTable("adjective"))
    } yield {
      assert(createResult == 3)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(bigAdjectiveOpt, clumsyAdjectiveOpt, highAdjectiveOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createAdjectiveTable *> createAdjectiveListSafely(list))
      adjectiveOpt <- runTransaction(readAdjectiveById(2))
      adjective <- IO.fromOption(adjectiveOpt)(throw new Exception("Adjective not found."))
      _ <- runTransaction(dropTable("adjective"))
    } yield {
      assert(adjective.value == "clumsy")
    }
  }
}