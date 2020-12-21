package edu.evolution.varanovich.anki.utility

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram.{dropTable, dropType}
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram._
import edu.evolution.varanovich.anki.db.program.entity.AnswerInfoProgram.createAnswerInfoTable
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.createCardTable
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.createDeckTable
import edu.evolution.varanovich.anki.db.program.entity.NounProgram._
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.{createPhraseListSafely, createPhraseTable}
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.{createPrepositionListSafely, createPrepositionTable}
import edu.evolution.varanovich.anki.db.program.entity.UserProgram.createUserTable
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.{createVerbListSafely, createVerbTable}
import edu.evolution.varanovich.anki.file.{DataParser, DataReader, FileAliases}


object DatabaseUtility extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    if(args.nonEmpty) {
      val mode = args.head
      for {
        _ <- mode match {
          case "init" => initDatabaseTables
          case "drop" => dropDatabaseTables
          case param => IO(println(s"Unknown parameter '$param'"))
        }
      } yield ExitCode.Success
    } else IO(println("Put arguments 'init' or 'drop'.")) *> IO(ExitCode.Error)

  }

  def initDatabaseTables: IO[Unit] = {
    val createVocabulary = createAdjectiveTable *> createNounTable *>
      createPhraseTable *> createPrepositionTable *> createVerbTable
    val createAnki = createUserTable *> createDeckTable *> createCardTable *> createAnswerInfoTable

    for {
      _ <- DbManager.transactorBlock(createVocabulary *> createAnki)
      adjectives <- DataReader.all(FileAliases.Adjective, DataParser.adjective)
      _ <- DbManager.transactor.use(createAdjectiveListSafely(adjectives.toList).transact[IO])
      nouns <- DataReader.all(FileAliases.Noun, DataParser.noun)
      _ <- DbManager.transactor.use(createNounListSafely(nouns.toList).transact[IO])
      phrases <- DataReader.all(FileAliases.Phrase, DataParser.phrase)
      _ <- DbManager.transactor.use(createPhraseListSafely(phrases.toList).transact[IO])
      prepositions <- DataReader.all(FileAliases.Preposition, DataParser.preposition)
      _ <- DbManager.transactor.use(createPrepositionListSafely(prepositions.toList).transact[IO])
      verbs <- DataReader.all(FileAliases.Verb, DataParser.verb)
      _ <- DbManager.transactor.use(createVerbListSafely(verbs.toList).transact[IO])
    } yield ()
  }

  def dropDatabaseTables: IO[Unit] = {
    val dropVocabulary = dropTable("adjective") *> dropTable("noun") *> dropTable("preposition") *>
      dropTable("phrase") *> dropTable("verb")
    val dropAnki = dropTable("answer_info") *> dropTable("card") *> dropTable("deck") *> dropTable("anki_user") *>
      dropType("privileges_enum") *> dropType("rate_enum")

    DbManager.transactorBlock(dropVocabulary *> dropAnki).map(_ => ())
  }
}