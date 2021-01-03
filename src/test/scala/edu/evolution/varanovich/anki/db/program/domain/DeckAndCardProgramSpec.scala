package edu.evolution.varanovich.anki.db.program.domain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.catsSyntaxApply
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.CardProgram._
import edu.evolution.varanovich.anki.db.program.domain.DeckProgram.{createDeck, createDeckTable, _}
import edu.evolution.varanovich.anki.db.program.domain.UserProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import edu.evolution.varanovich.anki.{deckOpt, _}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager.runTransaction

class DeckAndCardProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      _ <- runTransaction(createUserTable *> createUser(user))
      deck <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      userIdOpt <- runTransaction(readSequentialId(user.name))
      userId <- IO.fromOption(userIdOpt)(throw new Exception("User not found"))
      _ <- DbManager.runTransaction(createDeckTable *> createDeck(deck, userId))
      deckIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deck.description, user.name))
      deckId <- IO.fromOption(deckIdOpt)(throw new Exception("Deck not found"))
      _ <- runTransaction(createCardTable *> createCardList(deck.cards.toList, deckId))
      cardListResult <- runTransaction(readCardList(deckId))
      _ <- runTransaction(deleteDeck(deck))
      emptyCardList <- runTransaction(readCardList(deckId))
      dropCardTableResult <- runTransaction(dropTable("card"))
      dropDeckTableResult <- runTransaction(dropTable("deck"))
      dropUserTableResult <- runTransaction(dropTable("anki_user"))
      dropPrivilegesTypeResult <- runTransaction(dropType("privileges_enum"))
      dropRateTypeResult <- runTransaction(dropType("rate_enum"))
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

  "should successfully perform service operations" in {
    for {
      userOne <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      userTwo <- IO.fromOption(userOptSecond)(throw new Exception("User not created."))
      _ <- runTransaction(createUserTable *> createUser(userOne) *> createUser(userTwo))
      userOneIdOpt <- runTransaction(readSequentialId(userOne.name))
      userOneId <- IO.fromOption(userOneIdOpt)(throw new Exception("User not found"))
      userTwoIdOpt <- runTransaction(readSequentialId(userTwo.name))
      userTwoId <- IO.fromOption(userTwoIdOpt)(throw new Exception("User not found"))
      deckOne <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      deckTwo <- IO.fromOption(deckOptSecond)(throw new Exception("Deck not created"))
      deckThree <- IO.fromOption(deckOptThird)(throw new Exception("Deck not created"))
      _ <- runTransaction(createDeckTable *>
        createDeck(deckThree, userOneId) *>
        createDeck(deckOne, userTwoId) *>
        createDeck(deckTwo, userOneId) *>
        createDeck(deckTwo, userTwoId) *>
        createDeck(deckOne, userOneId) *>
        createDeck(deckThree, userTwoId)
      )

      deckOneUserOneIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckOne.description, userOne.name))
      deckOneUserOneId <- IO.fromOption(deckOneUserOneIdOpt)(throw new Exception("Deck not found"))
      deckTwoUserOneIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckTwo.description, userOne.name))
      deckTwoUserOneId <- IO.fromOption(deckTwoUserOneIdOpt)(throw new Exception("Deck not found"))
      deckThreeUserOneIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckThree.description, userOne.name))
      deckThreeUserOneId <- IO.fromOption(deckThreeUserOneIdOpt)(throw new Exception("Deck not found"))

      deckOneUserTwoIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckOne.description, userTwo.name))
      deckOneUserTwoId <- IO.fromOption(deckOneUserTwoIdOpt)(throw new Exception("Deck not found"))
      deckTwoUserTwoIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckTwo.description, userTwo.name))
      deckTwoUserTwoId <- IO.fromOption(deckTwoUserTwoIdOpt)(throw new Exception("Deck not found"))
      deckThreeUserTwoIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckThree.description, userTwo.name))
      deckThreeUserTwoId <- IO.fromOption(deckThreeUserTwoIdOpt)(throw new Exception("Deck not found"))

      _ <- runTransaction(createCardTable *>
        createCardList(deckOne.cards.toList, deckOneUserOneId) *>
        createCardList(deckTwo.cards.toList, deckTwoUserOneId) *>
        createCardList(deckThree.cards.toList, deckThreeUserOneId) *>
        createCardList(deckOne.cards.toList, deckOneUserTwoId) *>
        createCardList(deckTwo.cards.toList, deckTwoUserTwoId) *>
        createCardList(deckThree.cards.toList, deckThreeUserTwoId)
      )
      lastGeneratedUserOneInfoOpt <- runTransaction(
        readLastDeckInfoByPatternAndUserName(GeneratedDeckName, userOne.name))
      lastGeneratedUserOneInfo <- IO
        .fromOption(lastGeneratedUserOneInfoOpt)(throw new Exception("Last generated deck not found."))
      deckIdList <- runTransaction(readDeckIdListByUserName(userOne.name))
      cardIdOpt <- runTransaction(readCardIdByDeckIdAndContent(deckTwoUserTwoId, deckTwo.cards.toList.head))
      cardId <- IO.fromOption(cardIdOpt)(throw new Exception("Card id found."))
      generatedCardList <- runTransaction(readCardList(lastGeneratedUserOneInfo._1))
      _ <- runTransaction(dropTable("card"))
      _ <- runTransaction(dropTable("deck"))
      _ <- runTransaction(dropTable("anki_user"))
      _ <- runTransaction(dropType("privileges_enum"))
    } yield {
      assert(generatedCardList.toSet == deckTwo.cards)
      assert(deckIdList == List(1, 3, 5))
      assert(cardId == 26)
    }
  }
}