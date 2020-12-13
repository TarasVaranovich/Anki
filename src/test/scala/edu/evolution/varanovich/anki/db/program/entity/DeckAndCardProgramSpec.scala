package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram._
import edu.evolution.varanovich.anki.db.program.entity.CardProgram._
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.{createDeckTable, _}
import edu.evolution.varanovich.anki.db.program.entity.UserProgram._
import edu.evolution.varanovich.anki.{deckOpt, _}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class DeckAndCardProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      _ <- DbManager.transactorBlock(createUserTable *> createUser(user))
      deck <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      userIdOpt <- DbManager.transactor.use(readSequentialId(user.name).transact[IO])
      userId <- IO.fromOption(userIdOpt)(throw new Exception("User not found"))
      _ <- DbManager.transactorBlock(createDeckTable *> createDeck(deck, userId))
      deckIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deck.description, user.name).transact[IO])
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