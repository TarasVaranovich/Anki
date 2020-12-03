package edu.evolution.varanovich.anki.db.program

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki.{deckOpt, _}
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram._
import edu.evolution.varanovich.anki.db.program.entity.CardProgram._
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram._
import edu.evolution.varanovich.anki.db.program.entity.NounProgram._
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram._
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram._
import edu.evolution.varanovich.anki.db.program.entity.ServiceProgram._
import edu.evolution.varanovich.anki.db.program.entity.UserProgram._
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram._
import edu.evolution.varanovich.anki.utility.CryptoUtility.encryptSHA256
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class DtoProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations relating 'adjective' table" in {
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

  "should successfully perform 'CRUD' operations relating 'noun' table" in {
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

  "should successfully perform 'CRUD' operations relating 'phrase' table" in {
    for {
      phrase <- IO.fromOption(phraseOpt)(throw new Exception("Phrase not created."))
      insertResult <- DbManager.transactorBlock(createPhraseTable *> createPhrase(phrase))
      phraseModified <- IO.fromOption(phraseModifiedOpt)(throw new Exception("Modified phrase not created"))
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

  "should successfully perform 'CRUD' operations relating 'preposition' table" in {
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

  "should successfully perform 'CRUD' operations relating 'verb' table" in {
    for {
      verb <- IO.fromOption(consistVerbOpt)(throw new Exception("Verb not created."))
      insertResult <- DbManager.transactorBlock(createVerbTable *> createVerb(verb))
      verbModified <- IO.fromOption(consistVerbModifiedOpt)(throw new Exception("Modified verb not created"))
      updateResult <- DbManager.transactor.use(updateVerb(verbModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readVerb(verb.value).transact[IO])
      result <- IO.fromOption(resultOpt)(throw new Exception("Verb not found"))
      deleteResult <- DbManager.transactor.use(deleteVerb(verb).transact[IO])
      deleted <- DbManager.transactor.use(readVerb(verb.value).transact[IO])
      dropResult <- DbManager.transactor.use(dropTable("verb").transact[IO])
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.translation == verbModified.translation)
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropResult == 0)
    }
  }

  "should successfully perform 'CRUD' operations relating 'user' table" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      insertResult <- DbManager.transactorBlock(createUserTable *> createUser(user))
      userModified <- IO.fromOption(userModifiedOpt)(throw new Exception("Modified user not created"))
      updateResult <- DbManager.transactor.use(updateUser(userModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readUser(user.name).transact[IO])
      passwordOpt <- DbManager.transactor.use(readPassword(user.name).transact[IO])
      password <- IO.fromOption(passwordOpt)(throw new Exception("Modified user's password not found"))
      result <- IO.fromOption(resultOpt)(throw new Exception("User not found"))
      deleteResult <- DbManager.transactor.use(deleteUser(user).transact[IO])
      deleted <- DbManager.transactor.use(readUser(user.name).transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      dropTypeResult <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.privileges == userModified.privileges)
      assert(password == encryptSHA256(userModified.password))
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropTableResult == 0)
      assert(dropTypeResult == 0)
    }
  }

  "should successfully perform 'CRUD' operations relating 'deck' and 'card' tables" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      _ <- DbManager.transactorBlock(createUserTable *> createUser(user))
      deck <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      userIdOpt <- DbManager.transactor.use(readSequentialId(user.name).transact[IO])
      userId <- IO.fromOption(userIdOpt)(throw new Exception("User not found"))
      _ <- DbManager.transactorBlock(createDeckTable *> createDeck(deck, userId))
      deckIdOpt <- DbManager.transactor.use(readDeckId(deck.description).transact[IO])
      deckId <- IO.fromOption(deckIdOpt)(throw new Exception("Deck not found"))
      _ <- DbManager.transactorBlock(createCardTable *> createCardList(deck.cards.toList, deckId))
      cardListResult <- DbManager.transactor.use(readCardList(deckId).transact[IO])
      _ <- DbManager.transactor.use(deleteDeck(deck).transact[IO])
      emptyCardList <- DbManager.transactor.use(readCardList(deckId).transact[IO])
      dropCardTableResult <- DbManager.transactor.use(dropTable("card").transact[IO])
      dropDeckTableResult <- DbManager.transactor.use(dropTable("deck").transact[IO])
      dropUserTableResult <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      dropTypeResult <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
    } yield {
      assert(cardListResult.toSet == deck.cards)
      assert(emptyCardList.isEmpty)
      assert(dropCardTableResult == 0)
      assert(dropDeckTableResult == 0)
      assert(dropUserTableResult == 0)
      assert(dropTypeResult == 0)
    }
  }
}