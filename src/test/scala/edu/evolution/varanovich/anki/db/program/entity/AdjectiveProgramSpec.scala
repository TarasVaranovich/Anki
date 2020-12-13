package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram._
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class AdjectiveProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      adjective <- IO.fromOption(highAdjectiveOpt)(throw new Exception("Adjective not created."))
      insertResult <- DbManager.transactorBlock(createAdjectiveTable *> createAdjective(adjective))
      adjectiveModified <- IO.fromOption(highAdjectiveModifiedOpt)(throw new Exception("Modified adjective not created"))
      updateResult <- DbManager.transactor.use(updateAdjective(adjectiveModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readAdjective(adjective.value).transact[IO])
      result <- IO.fromOption(resultOpt)(throw new Exception("Adjective not found"))
      deleteResult <- DbManager.transactor.use(deleteAdjective(adjective).transact[IO])
      deleted <- DbManager.transactor.use(readAdjective(adjective.value).transact[IO])
      dropResult <- DbManager.transactor.use(dropTable("adjective").transact[IO])
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
      createResult <- DbManager.transactorBlock(createAdjectiveTable *> createAdjectiveListSafely(list))
      listResult <- DbManager.transactor.use(readAllAdjectives.transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("adjective").transact[IO])
    } yield {
      assert(createResult == 3)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(bigAdjectiveOpt, clumsyAdjectiveOpt, highAdjectiveOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createAdjectiveTable *> createAdjectiveListSafely(list))
      adjectiveOpt <- DbManager.transactor.use(readAdjectiveById(2).transact[IO])
      adjective <- IO.fromOption(adjectiveOpt)(throw new Exception("Adjective not found."))
      _ <- DbManager.transactor.use(dropTable("adjective").transact[IO])
    } yield {
      assert(adjective.value == "clumsy")
    }
  }
}