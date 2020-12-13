package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram._
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class VerbProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      verb <- IO.fromOption(consistVerbOpt)(throw new Exception("Verb not created."))
      insertResult <- DbManager.transactorBlock(createVerbTable *> createVerb(verb))
      verbModified <- IO.fromOption(consistVerbModifiedOpt)(throw new Exception("Modified verb not created"))
      updateResult <- DbManager.transactor.use(updateVerb(verbModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readVerb(verb.value).transact[IO])
      result <- IO.fromOption(resultOpt)(throw new Exception("Verb not found"))
      deleteResult <- DbManager.transactor.use(deleteVerb(verb).transact[IO])
      deleted <- DbManager.transactor.use(readVerb(verb.value).transact[IO])
      dropResult <- DbManager.transactor.use(dropTable("verb").transact[IO])
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
      createResult <- DbManager.transactorBlock(createVerbTable *> createVerbListSafely(list))
      listResult <- DbManager.transactor.use(readAllVerbs.transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("verb").transact[IO])
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createVerbTable *> createVerbListSafely(list))
      phraseOpt <- DbManager.transactor.use(readVerbById(2).transact[IO])
      phrase <- IO.fromOption(phraseOpt)(throw new Exception("Verb not found."))
      _ <- DbManager.transactor.use(dropTable("verb").transact[IO])
    } yield {
      assert(phrase.value == "discover")
    }
  }
}