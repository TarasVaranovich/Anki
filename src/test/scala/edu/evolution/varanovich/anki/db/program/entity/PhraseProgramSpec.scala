package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.{createNounListSafely, createNounTable, readNounById}
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram._
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class PhraseProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      phrase <- IO.fromOption(howAreYouPhraseOpt)(throw new Exception("Phrase not created."))
      insertResult <- DbManager.transactorBlock(createPhraseTable *> createPhrase(phrase))
      phraseModified <- IO.fromOption(howAreYouPhraseModifiedOpt)(throw new Exception("Modified phrase not created"))
      updateResult <- DbManager.transactor.use(updatePhrase(phraseModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readPhrase(phrase.value).transact[IO])
      result <- IO.fromOption(resultOpt)(throw new Exception("Phrase not found"))
      deleteResult <- DbManager.transactor.use(deletePhrase(phrase).transact[IO])
      deleted <- DbManager.transactor.use(readPhrase(phrase.value).transact[IO])
      dropResult <- DbManager.transactor.use(dropTable("phrase").transact[IO])
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.translation == phraseModified.translation)
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropResult == 0)
    }
  }

  "should successfully perform list operations" in {
    for {
      list <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      createResult <- DbManager.transactorBlock(createPhraseTable *> createPhraseListSafely(list))
      listResult <- DbManager.transactor.use(readAllPhrases.transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("phrase").transact[IO])
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createPhraseTable *> createPhraseListSafely(list))
      phraseOpt <- DbManager.transactor.use(readPhraseById(2).transact[IO])
      phrase <- IO.fromOption(phraseOpt)(throw new Exception("Phrase not found."))
      _ <- DbManager.transactor.use(dropTable("phrase").transact[IO])
    } yield {
      assert(phrase.value == "window of vulnerability")
    }
  }
}