package edu.evolution.varanovich.anki.db.program.service

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.{catsSyntaxApply, toTraverseOps}
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.{createAdjectiveListSafely, createAdjectiveTable}
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.{createNounListSafely, createNounTable}
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.{createPhraseListSafely, createPhraseTable}
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.{createPrepositionListSafely, createPrepositionTable}
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.{createVerbListSafely, createVerbTable}
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram.{dropTable, readMaxId, readRowsCount}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers

class ServiceProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully read part of speech max identifiers" in {
    for {
      adjectiveList <- IO(List(bigAdjectiveOpt, clumsyAdjectiveOpt, highAdjectiveOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createAdjectiveTable *> createAdjectiveListSafely(adjectiveList))
      adjectiveMaxIdOpt <- runTransaction(readMaxId("adjective"))
      adjectiveMaxId <- IO.fromOption(adjectiveMaxIdOpt)(throw new Exception("Adjectives absent."))
      nounList <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createNounTable *> createNounListSafely(nounList))
      nounMaxIdOpt <- runTransaction(readMaxId("noun"))
      nounMaxId <- IO.fromOption(nounMaxIdOpt)(throw new Exception("Nouns absent."))
      phraseList <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createPhraseTable *> createPhraseListSafely(phraseList))
      phraseMaxIdOpt <- runTransaction(readMaxId("phrase"))
      phraseMaxId <- IO.fromOption(phraseMaxIdOpt)(throw new Exception("Phrases absent."))
      prepositionList <- IO(List(abovePrepositionOpt, betweenPrepositionOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createPrepositionTable *> createPrepositionListSafely(prepositionList))
      prepositionMaxIdOpt <- runTransaction(readMaxId("preposition"))
      prepositionMaxId <- IO.fromOption(prepositionMaxIdOpt)(throw new Exception("Prepositions absent."))
      verbList <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createVerbTable *> createVerbListSafely(verbList))
      verbMaxIdOpt <- runTransaction(readMaxId("verb"))
      verbMaxId <- IO.fromOption(verbMaxIdOpt)(throw new Exception("Verb absent."))
      _ <- runTransaction(dropTable("adjective"))
      _ <- runTransaction(dropTable("noun"))
      _ <- runTransaction(dropTable("phrase"))
      _ <- runTransaction(dropTable("preposition"))
      _ <- runTransaction(dropTable("verb"))
    } yield {
      assert(adjectiveMaxId == adjectiveList.length)
      assert(nounMaxId == nounList.length)
      assert(phraseMaxId == phraseList.length)
      assert(prepositionMaxId == prepositionList.length)
      assert(verbMaxId == verbList.length)
    }
  }

  "should successfully read rows count" in {
    for {
      adjectiveList <- IO(List(bigAdjectiveOpt, clumsyAdjectiveOpt, highAdjectiveOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createAdjectiveTable *> createAdjectiveListSafely(adjectiveList))
      adjectiveCountOpt <- runTransaction(readRowsCount("adjective"))
      adjectiveCount <- IO.fromOption(adjectiveCountOpt)(throw new Exception("Adjective absent."))
      nounList <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createNounTable *> createNounListSafely(nounList))
      nounCountOpt <-runTransaction(readRowsCount("noun"))
      nounCount <- IO.fromOption(nounCountOpt)(throw new Exception("Noun absent."))
      phraseList <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createPhraseTable *> createPhraseListSafely(phraseList))
      phraseCountOpt <- runTransaction(readRowsCount("phrase"))
      phraseCount <- IO.fromOption(phraseCountOpt)(throw new Exception("Phrase absent."))
      prepositionList <- IO(List(abovePrepositionOpt, betweenPrepositionOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createPrepositionTable *> createPrepositionListSafely(prepositionList))
      prepositionCountOpt <- runTransaction(readRowsCount("preposition"))
      prepositionCount <- IO.fromOption(prepositionCountOpt)(throw new Exception("Preposition absent."))
      verbList <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      _ <- runTransaction(createVerbTable *> createVerbListSafely(verbList))
      verbCountOpt <- runTransaction(readRowsCount("verb"))
      verbCount <- IO.fromOption(verbCountOpt)(throw new Exception("Verb absent."))
      _ <- runTransaction(dropTable("adjective"))
      _ <- runTransaction(dropTable("noun"))
      _ <- runTransaction(dropTable("phrase"))
      _ <- runTransaction(dropTable("preposition"))
      _ <- runTransaction(dropTable("verb"))
    } yield {
      assert(adjectiveCount == adjectiveList.size)
      assert(nounCount == nounList.size)
      assert(phraseCount == phraseList.size)
      assert(prepositionCount == prepositionList.size)
      assert(verbCount == verbList.size)
    }
  }
}