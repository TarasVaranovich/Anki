package edu.evolution.varanovich.anki.utility

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxApply
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.createAdjectiveTable
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.createCardTable
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.createDeckTable
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.createNounTable
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.createPhraseTable
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.createPrepositionTable
import edu.evolution.varanovich.anki.db.program.entity.ServiceProgram.{dropTable, dropType}
import edu.evolution.varanovich.anki.db.program.entity.UserProgram.createUserTable
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.createVerbTable

//TODO: apply parallel execution on vocabulary tables
//TODO: create command line application which accepts parameters
object DatabaseUtility extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val createVocabulary = createAdjectiveTable *> createNounTable *>
      createPhraseTable *> createPrepositionTable *> createVerbTable
    val createAnki = createUserTable *> createDeckTable *> createCardTable
    val dropVocabulary = dropTable("adjective") *> dropTable("noun") *> dropTable("preposition") *>
      dropTable("phrase") *> dropTable("verb")
    val dropAnki = dropTable("card") *> dropTable("deck") *> dropTable("anki_user") *> dropType("privileges_enum")
    for {
      //_ <- DbManager.transactorBlock(createVocabulary *> createAnki)
      _ <- DbManager.transactorBlock(dropVocabulary *> dropAnki)
    } yield ExitCode.Success
  }
}
