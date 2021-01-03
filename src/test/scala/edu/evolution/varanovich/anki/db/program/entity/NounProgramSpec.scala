package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.{createAdjectiveListSafely, createAdjectiveTable, readAdjectiveById}
import edu.evolution.varanovich.anki.db.program.entity.NounProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class NounProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      noun <- IO.fromOption(coastNounOpt)(throw new Exception("Noun not created."))
      insertResult <- runTransaction(createNounTable *> createNoun(noun))
      nounModified <- IO.fromOption(coastNounModifiedOpt)(throw new Exception("Modified noun not created"))
      updateResult <- runTransaction(updateNoun(nounModified))
      resultOpt <- runTransaction(readNoun(noun.value))
      result <- IO.fromOption(resultOpt)(throw new Exception("Noun not found"))
      deleteResult <- runTransaction(deleteNoun(noun))
      deleted <- runTransaction(readNoun(noun.value))
      dropResult <- runTransaction(dropTable("noun"))
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.translation == nounModified.translation)
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropResult == 0)
    }
  }

  "should successfully perform list operations" in {
    for {
      list <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      createResult <- runTransaction(createNounTable *> createNounListSafely(list))
      listResult <- runTransaction(readAllNouns)
      dropTableResult <- runTransaction(dropTable("noun"))
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createNounTable *> createNounListSafely(list))
      nounOpt <- runTransaction(readNounById(2))
      noun <- IO.fromOption(nounOpt)(throw new Exception("Noun not found."))
      _ <- runTransaction(dropTable("noun"))
    } yield {
      assert(noun.value == "glasses")
    }
  }
}