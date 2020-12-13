package edu.evolution.varanovich.anki.db.program.domain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.{catsSyntaxApply, toTraverseOps}
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram.{dropTable, readMaxId, readRowsCount}
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.{createAdjectiveListSafely, createAdjectiveTable}
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.{createNounListSafely, createNounTable}
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.{createPhraseListSafely, createPhraseTable}
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.{createPrepositionListSafely, createPrepositionTable}
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.{createVerbListSafely, createVerbTable}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers

class ServiceProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully read part of speech max identifiers" in {
    for {
      adjectiveList <- IO(List(bigAdjectiveOpt, clumsyAdjectiveOpt, highAdjectiveOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createAdjectiveTable *> createAdjectiveListSafely(adjectiveList))
      adjectiveMaxIdOpt <- DbManager.transactor.use(readMaxId("adjective").transact[IO])
      adjectiveMaxId <- IO.fromOption(adjectiveMaxIdOpt)(throw new Exception("Adjectives absent."))
      nounList <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createNounTable *> createNounListSafely(nounList))
      nounMaxIdOpt <- DbManager.transactor.use(readMaxId("noun").transact[IO])
      nounMaxId <- IO.fromOption(nounMaxIdOpt)(throw new Exception("Nouns absent."))
      phraseList <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createPhraseTable *> createPhraseListSafely(phraseList))
      phraseMaxIdOpt <- DbManager.transactor.use(readMaxId("phrase").transact[IO])
      phraseMaxId <- IO.fromOption(phraseMaxIdOpt)(throw new Exception("Phrases absent."))
      prepositionList <- IO(List(abovePrepositionOpt, betweenPrepositionOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createPrepositionTable *> createPrepositionListSafely(prepositionList))
      prepositionMaxIdOpt <- DbManager.transactor.use(readMaxId("preposition").transact[IO])
      prepositionMaxId <- IO.fromOption(prepositionMaxIdOpt)(throw new Exception("Prepositions absent."))
      verbList <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createVerbTable *> createVerbListSafely(verbList))
      verbMaxIdOpt <- DbManager.transactor.use(readMaxId("verb").transact[IO])
      verbMaxId <- IO.fromOption(verbMaxIdOpt)(throw new Exception("Verb absent."))
      _ <- DbManager.transactor.use(dropTable("adjective").transact[IO])
      _ <- DbManager.transactor.use(dropTable("noun").transact[IO])
      _ <- DbManager.transactor.use(dropTable("phrase").transact[IO])
      _ <- DbManager.transactor.use(dropTable("preposition").transact[IO])
      _ <- DbManager.transactor.use(dropTable("verb").transact[IO])
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
      _ <- DbManager.transactorBlock(createAdjectiveTable *> createAdjectiveListSafely(adjectiveList))
      adjectiveCountOpt <- DbManager.transactor.use(readRowsCount("adjective").transact[IO])
      adjectiveCount <- IO.fromOption(adjectiveCountOpt)(throw new Exception("Adjective absent."))
      nounList <- IO(List(coastNounOpt, glassesNounOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createNounTable *> createNounListSafely(nounList))
      nounCountOpt <- DbManager.transactor.use(readRowsCount("noun").transact[IO])
      nounCount <- IO.fromOption(nounCountOpt)(throw new Exception("Noun absent."))
      phraseList <- IO(List(howAreYouPhraseOpt, windowOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createPhraseTable *> createPhraseListSafely(phraseList))
      phraseCountOpt <- DbManager.transactor.use(readRowsCount("phrase").transact[IO])
      phraseCount <- IO.fromOption(phraseCountOpt)(throw new Exception("Phrase absent."))
      prepositionList <- IO(List(abovePrepositionOpt, betweenPrepositionOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createPrepositionTable *> createPrepositionListSafely(prepositionList))
      prepositionCountOpt <- DbManager.transactor.use(readRowsCount("preposition").transact[IO])
      prepositionCount <- IO.fromOption(prepositionCountOpt)(throw new Exception("Preposition absent."))
      verbList <- IO(List(consistVerbOpt, discoverVerbOpt).sequence.getOrElse(List()))
      _ <- DbManager.transactorBlock(createVerbTable *> createVerbListSafely(verbList))
      verbCountOpt <- DbManager.transactor.use(readRowsCount("verb").transact[IO])
      verbCount <- IO.fromOption(verbCountOpt)(throw new Exception("Verb absent."))
      _ <- DbManager.transactor.use(dropTable("adjective").transact[IO])
      _ <- DbManager.transactor.use(dropTable("noun").transact[IO])
      _ <- DbManager.transactor.use(dropTable("phrase").transact[IO])
      _ <- DbManager.transactor.use(dropTable("preposition").transact[IO])
      _ <- DbManager.transactor.use(dropTable("verb").transact[IO])
    } yield {
      assert(adjectiveCount == adjectiveList.size)
      assert(nounCount == nounList.size)
      assert(phraseCount == phraseList.size)
      assert(prepositionCount == prepositionList.size)
      assert(verbCount == verbList.size)
    }
  }
}