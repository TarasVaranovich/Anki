package edu.evolution.varanovich.anki.utility

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import doobie.Transactor
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.domain.AnswerInfoProgram.createAnswerInfoTable
import edu.evolution.varanovich.anki.db.program.domain.CardProgram.createCardTable
import edu.evolution.varanovich.anki.db.program.domain.DeckProgram.createDeckTable
import edu.evolution.varanovich.anki.db.program.domain.UserProgram.createUserTable
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram._
import edu.evolution.varanovich.anki.db.program.entity.NounProgram._
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.{createPhraseListSafely, createPhraseTable}
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.{createPrepositionListSafely, createPrepositionTable}
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.{createVerbListSafely, createVerbTable}
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram.{dropTable, dropType}
import edu.evolution.varanovich.anki.file.{DataParser, DataReader, FileAliases}


object DatabaseUtility extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(DbManager.transactorInstance).flatMap(transactor =>
      if (args.nonEmpty) {
        (args.head match {
          case "init" => initDatabaseTables(transactor)
          case "drop" => dropDatabaseTables(transactor)
          case param => IO(println(s"Unknown parameter '$param'"))
        }) *> IO(ExitCode.Success)
      } else IO(println("Put arguments 'init' or 'drop'.")) *> IO(ExitCode.Error)
    )
  }

  def initDatabaseTables(implicit transactor: Resource[IO, Transactor[IO]]): IO[Unit] = {
    val createVocabulary = createAdjectiveTable *> createNounTable *>
      createPhraseTable *> createPrepositionTable *> createVerbTable
    val createAnki = createUserTable *> createDeckTable *> createCardTable *> createAnswerInfoTable

    for {
      _ <- runTransaction(createVocabulary *> createAnki)
      adjectives <- DataReader.all(FileAliases.Adjective, DataParser.adjective)
      _ <- runTransaction(createAdjectiveListSafely(adjectives.toList))
      nouns <- DataReader.all(FileAliases.Noun, DataParser.noun)
      _ <- runTransaction(createNounListSafely(nouns.toList))
      phrases <- DataReader.all(FileAliases.Phrase, DataParser.phrase)
      _ <- runTransaction(createPhraseListSafely(phrases.toList))
      prepositions <- DataReader.all(FileAliases.Preposition, DataParser.preposition)
      _ <- runTransaction(createPrepositionListSafely(prepositions.toList))
      verbs <- DataReader.all(FileAliases.Verb, DataParser.verb)
      _ <- DbManager.transactorInstance.use(createVerbListSafely(verbs.toList).transact[IO])
    } yield ()
  }

  def dropDatabaseTables(implicit transactor: Resource[IO, Transactor[IO]]): IO[Unit] = {
    val dropVocabulary = dropTable("adjective") *> dropTable("noun") *> dropTable("preposition") *>
      dropTable("phrase") *> dropTable("verb")
    val dropAnki = dropTable("answer_info") *> dropTable("card") *> dropTable("deck") *>
      dropTable("anki_user") *> dropType("privileges_enum") *> dropType("rate_enum")

    runTransaction(dropVocabulary *> dropAnki).map(_ => ())
  }
}