package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class PrepositionProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      preposition <- IO.fromOption(abovePrepositionOpt)(throw new Exception("Preposition not created."))
      insertResult <- runTransaction(createPrepositionTable *> createPreposition(preposition))
      prepositionModified <- IO.fromOption(abovePrepositionModifiedOpt)(throw new Exception("Modified preposition not created"))
      updateResult <- runTransaction(updatePreposition(prepositionModified))
      resultOpt <- runTransaction(readPreposition(preposition.value))
      result <- IO.fromOption(resultOpt)(throw new Exception("Preposition not found"))
      deleteResult <- runTransaction(deletePreposition(preposition))
      deleted <- runTransaction(readPreposition(preposition.value))
      dropResult <- runTransaction(dropTable("preposition"))
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.translation == prepositionModified.translation)
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropResult == 0)
    }
  }

  "should successfully perform list operations" in {
    for {
      list <- IO(List(abovePrepositionOpt, betweenPrepositionOpt).sequence.getOrElse(List()))
      createResult <- runTransaction(createPrepositionTable *> createPrepositionListSafely(list))
      listResult <- runTransaction(readAllPrepositions)
      dropTableResult <- runTransaction(dropTable("preposition"))
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(abovePrepositionOpt, betweenPrepositionOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createPrepositionTable *> createPrepositionListSafely(list))
      phraseOpt <- runTransaction(readPrepositionById(2))
      phrase <- IO.fromOption(phraseOpt)(throw new Exception("Preposition not found."))
      _ <- runTransaction(dropTable("preposition"))
    } yield {
      assert(phrase.value == "between")
    }
  }
}