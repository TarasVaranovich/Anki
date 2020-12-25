package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.{createAdjectiveListSafely, createAdjectiveTable, readAdjectiveById}
import edu.evolution.varanovich.anki.db.program.entity.NounProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class NounProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      noun <- IO.fromOption(coastNounOpt)(throw new Exception("Noun not created."))
      insertResult <- DbManager.transactorBlock(createNounTable *> createNoun(noun))
      nounModified <- IO.fromOption(coastNounModifiedOpt)(throw new Exception("Modified noun not created"))
      updateResult <- DbManager.transactor.use(updateNoun(nounModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readNoun(noun.value).transact[IO])
      result <- IO.fromOption(resultOpt)(throw new Exception("Noun not found"))
      deleteResult <- DbManager.transactor.use(deleteNoun(noun).transact[IO])
      deleted <- DbManager.transactor.use(readNoun(noun.value).transact[IO])
      dropResult <- DbManager.transactor.use(dropTable("noun").transact[IO])
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
      createResult <- DbManager.transactorBlock(createNounTable *> createNounListSafely(list))
      listResult <- DbManager.transactor.use(readAllNouns.transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("noun").transact[IO])
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createNounTable *> createNounListSafely(list))
      nounOpt <- DbManager.transactor.use(readNounById(2).transact[IO])
      noun <- IO.fromOption(nounOpt)(throw new Exception("Noun not found."))
      _ <- DbManager.transactor.use(dropTable("noun").transact[IO])
    } yield {
      assert(noun.value == "glasses")
    }
  }
}