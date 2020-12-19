package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram._
import edu.evolution.varanovich.anki.db.program.entity.CardProgram._
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.{createDeck, createDeckTable, readEarliestFreshDeckInfo, _}
import edu.evolution.varanovich.anki.db.program.entity.UserProgram._
import edu.evolution.varanovich.anki.domain.DeckBuilder.GeneratedDeckName
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
      dropPrivilegesTypeResult <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
      dropRateTypeResult <- DbManager.transactor.use(dropType("rate_enum").transact[IO])
    } yield {
      assert(cardListResult.toSet == deck.cards)
      assert(emptyCardList.isEmpty)
      assert(dropCardTableResult == 0)
      assert(dropDeckTableResult == 0)
      assert(dropUserTableResult == 0)
      assert(dropPrivilegesTypeResult == 0)
      assert(dropRateTypeResult == 0)
    }
  }

  "should successfully read id of last generated deck" in {
    for {
      userOne <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      userTwo <- IO.fromOption(userOptSecond)(throw new Exception("User not created."))
      _ <- DbManager.transactorBlock(createUserTable *> createUser(userOne) *> createUser(userTwo))
      userOneIdOpt <- DbManager.transactor.use(readSequentialId(userOne.name).transact[IO])
      userOneId <- IO.fromOption(userOneIdOpt)(throw new Exception("User not found"))
      userTwoIdOpt <- DbManager.transactor.use(readSequentialId(userTwo.name).transact[IO])
      userTwoId <- IO.fromOption(userTwoIdOpt)(throw new Exception("User not found"))
      deckOne <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      deckTwo <- IO.fromOption(deckOptSecond)(throw new Exception("Deck not created"))
      deckThree <- IO.fromOption(deckOptThird)(throw new Exception("Deck not created"))
      _ <- DbManager.transactorBlock(createDeckTable *>
        createDeck(deckThree, userOneId) *>
        createDeck(deckOne, userTwoId) *>
        createDeck(deckTwo, userOneId) *>
        createDeck(deckTwo, userTwoId) *>
        createDeck(deckOne, userOneId) *>
        createDeck(deckThree, userTwoId)
      )

      deckOneUserOneIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deckOne.description, userOne.name).transact[IO])
      deckOneUserOneId <- IO.fromOption(deckOneUserOneIdOpt)(throw new Exception("Deck not found"))
      deckTwoUserOneIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deckTwo.description, userOne.name).transact[IO])
      deckTwoUserOneId <- IO.fromOption(deckTwoUserOneIdOpt)(throw new Exception("Deck not found"))
      deckThreeUserOneIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deckThree.description, userOne.name).transact[IO])
      deckThreeUserOneId <- IO.fromOption(deckThreeUserOneIdOpt)(throw new Exception("Deck not found"))

      deckOneUserTwoIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deckOne.description, userTwo.name).transact[IO])
      deckOneUserTwoId <- IO.fromOption(deckOneUserTwoIdOpt)(throw new Exception("Deck not found"))
      deckTwoUserTwoIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deckTwo.description, userTwo.name).transact[IO])
      deckTwoUserTwoId <- IO.fromOption(deckTwoUserTwoIdOpt)(throw new Exception("Deck not found"))
      deckThreeUserTwoIdOpt <- DbManager.transactor
        .use(readDeckIdByDescriptionAndUserName(deckThree.description, userTwo.name).transact[IO])
      deckThreeUserTwoId <- IO.fromOption(deckThreeUserTwoIdOpt)(throw new Exception("Deck not found"))

      _ <- DbManager.transactorBlock(createCardTable *>
        createCardList(deckOne.cards.toList, deckOneUserOneId) *>
        createCardList(deckTwo.cards.toList, deckTwoUserOneId) *>
        createCardList(deckThree.cards.toList, deckThreeUserOneId) *>
        createCardList(deckOne.cards.toList, deckOneUserTwoId) *>
        createCardList(deckTwo.cards.toList, deckTwoUserTwoId) *>
        createCardList(deckThree.cards.toList, deckThreeUserTwoId)
      )
      lastGeneratedUserOneInfoOpt <- DbManager.
        transactor.use(readLastDeckInfoByPatternAndUserName(GeneratedDeckName, userOne.name).transact[IO])
      lastGeneratedUserOneInfo <- IO
        .fromOption(lastGeneratedUserOneInfoOpt)(throw new Exception("Last generated deck ont found."))
      generatedCardList <- DbManager.transactor.use(readCardList(lastGeneratedUserOneInfo._1).transact[IO])
      _ <- DbManager.transactor.use(dropTable("card").transact[IO])
      _ <- DbManager.transactor.use(dropTable("deck").transact[IO])
      _ <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      _ <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
    } yield {
      assert(generatedCardList.toSet == deckTwo.cards)
    }
  }
}