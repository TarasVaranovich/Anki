package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.{createNounListSafely, createNounTable, readNounById}
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class PhraseProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      phrase <- IO.fromOption(howAreYouPhraseOpt)(throw new Exception("Phrase not created."))
      insertResult <- runTransaction(createPhraseTable *> createPhrase(phrase))
      phraseModified <- IO.fromOption(howAreYouPhraseModifiedOpt)(throw new Exception("Modified phrase not created"))
      updateResult <- runTransaction(updatePhrase(phraseModified))
      resultOpt <- runTransaction(readPhrase(phrase.value))
      result <- IO.fromOption(resultOpt)(throw new Exception("Phrase not found"))
      deleteResult <- runTransaction(deletePhrase(phrase))
      deleted <- runTransaction(readPhrase(phrase.value))
      dropResult <- runTransaction(dropTable("phrase"))
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
      createResult <- runTransaction(createPhraseTable *> createPhraseListSafely(list))
      listResult <- runTransaction(readAllPhrases)
      dropTableResult <- runTransaction(dropTable("phrase"))
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }

  "should successfully read by identifier" in {
    for {
      list <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createPhraseTable *> createPhraseListSafely(list))
      phraseOpt <- runTransaction(readPhraseById(2))
      phrase <- IO.fromOption(phraseOpt)(throw new Exception("Phrase not found."))
      _ <- runTransaction(dropTable("phrase"))
    } yield {
      assert(phrase.value == "window of vulnerability")
    }
  }
}