package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class VerbProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      verb <- IO.fromOption(consistVerbOpt)(throw new Exception("Verb not created."))
      insertResult <- runTransaction(createVerbTable *> createVerb(verb))
      verbModified <- IO.fromOption(consistVerbModifiedOpt)(throw new Exception("Modified verb not created"))
      updateResult <- runTransaction(updateVerb(verbModified))
      resultOpt <- runTransaction(readVerb(verb.value))
      result <- IO.fromOption(resultOpt)(throw new Exception("Verb not found"))
      deleteResult <- runTransaction(deleteVerb(verb))
      deleted <- runTransaction(readVerb(verb.value))
      dropResult <- runTransaction(dropTable("verb"))
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.translation == verbModified.translation)
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropResult == 0)
    }
  }

  "should successfully perform list operations" in {
    for {
      list <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      createResult <- runTransaction(createVerbTable *> createVerbListSafely(list))
      listResult <- runTransaction(readAllVerbs)
      dropTableResult <- runTransaction(dropTable("verb"))
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createVerbTable *> createVerbListSafely(list))
      phraseOpt <- runTransaction(readVerbById(2))
      phrase <- IO.fromOption(phraseOpt)(throw new Exception("Verb not found."))
      _ <- runTransaction(dropTable("verb"))
    } yield {
      assert(phrase.value == "discover")
    }
  }
}