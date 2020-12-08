package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram._
import edu.evolution.varanovich.anki.db.program.entity.ServiceProgram._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class PrepositionProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      preposition <- IO.fromOption(abovePrepositionOpt)(throw new Exception("Preposition not created."))
      insertResult <- DbManager.transactorBlock(createPrepositionTable *> createPreposition(preposition))
      prepositionModified <- IO.fromOption(abovePrepositionModifiedOpt)(throw new Exception("Modified preposition not created"))
      updateResult <- DbManager.transactor.use(updatePreposition(prepositionModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readPreposition(preposition.value).transact[IO])
      result <- IO.fromOption(resultOpt)(throw new Exception("Preposition not found"))
      deleteResult <- DbManager.transactor.use(deletePreposition(preposition).transact[IO])
      deleted <- DbManager.transactor.use(readPreposition(preposition.value).transact[IO])
      dropResult <- DbManager.transactor.use(dropTable("preposition").transact[IO])
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
      createResult <- DbManager.transactorBlock(createPrepositionTable *> createPrepositionListSafely(list))
      listResult <- DbManager.transactor.use(readAllPrepositions.transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("preposition").transact[IO])
    } yield {
      assert(createResult == 2)
      assert(listResult == list)
      assert(dropTableResult == 0)
    }
  }
}
