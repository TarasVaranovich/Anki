package edu.evolution.varanovich.anki.db.program.domain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.catsSyntaxApply
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.AnswerInfoProgram.{createAnswerInfo, createAnswerInfoTable, readAnswerInfoList}
import edu.evolution.varanovich.anki.db.program.domain.CardProgram._
import edu.evolution.varanovich.anki.db.program.domain.DeckProgram._
import edu.evolution.varanovich.anki.db.program.domain.UserProgram.{createUser, createUserTable, readSequentialId}
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram.{dropTable, dropType}
import edu.evolution.varanovich.anki.model.{AnswerInfo, Rate}
import edu.evolution.varanovich.anki.utility.AnkiConfig.MaxAnswerDuration
import edu.evolution.varanovich.anki.{deckOpt, deckOptSecond, deckOptThird, userOpt, userOptSecond}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import doobie.implicits._
import edu.evolution.varanovich.anki.model.Rate.{Easy, Fail, Good, Hard}

class AnswerInfoProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully create and read answer infos" in {
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

      firstCardIdOpt <- DbManager.transactor.use(readCardIdByDeckIdAndContent(deckId, deck.cards.head).transact[IO])
      firstCardId <- IO.fromOption(firstCardIdOpt)(throw new Exception("Card id not found"))
      firstAnswerInfoOne <- IO.fromOption(
        AnswerInfo.from(Rate.Fail, MaxAnswerDuration))(throw new Exception("Answer Info not created"))
      firstAnswerInfoTwo <- IO.fromOption(
        AnswerInfo.from(Rate.Good, 10))(throw new Exception("Answer Info not created"))

      lastCardIdOpt <- DbManager.transactor.use(readCardIdByDeckIdAndContent(deckId, deck.cards.last).transact[IO])
      lastCardId <- IO.fromOption(lastCardIdOpt)(throw new Exception("Card id not found"))
      lastAnswerInfoOne <- IO.fromOption(
        AnswerInfo.from(Rate.Hard, 100))(throw new Exception("Answer Info not created"))
      lastAnswerInfoTwo <- IO.fromOption(
        AnswerInfo.from(Rate.Easy, 3))(throw new Exception("Answer Info not created"))

      _ <- DbManager.transactorBlock(createAnswerInfoTable *>
        createAnswerInfo(firstAnswerInfoOne, firstCardId) *>
        createAnswerInfo(firstAnswerInfoTwo, firstCardId) *>
        createAnswerInfo(lastAnswerInfoOne, lastCardId) *>
        createAnswerInfo(lastAnswerInfoTwo, lastCardId))

      firstAnswerInfoList <- DbManager.transactor.use(readAnswerInfoList(firstCardId).transact[IO])
      lastAnswerInfoList <- DbManager.transactor.use(readAnswerInfoList(lastCardId).transact[IO])

      _ <- DbManager.transactor.use(dropTable("answer_info").transact[IO])
      _ <- DbManager.transactor.use(dropTable("card").transact[IO])
      _ <- DbManager.transactor.use(dropTable("deck").transact[IO])
      _ <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      _ <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
      _ <- DbManager.transactor.use(dropType("rate_enum").transact[IO])
    } yield {
      assert(firstAnswerInfoList.size == 2)
      assert(lastAnswerInfoList.size == 2)
    }
  }

  /**
   * Name convention for cards identifiers in test:
   * U - number of user
   * D - number of deck
   * C - number of card in deck
   */
  "should successfully perform analytics operations" in {
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
        createDeck(deckOne, userOneId)
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

      _ <- DbManager.transactorBlock(createCardTable *>
        createCardList(deckOne.cards.toList, deckOneUserOneId) *>
        createCardList(deckTwo.cards.toList, deckTwoUserOneId) *>
        createCardList(deckThree.cards.toList, deckThreeUserOneId) *>
        createCardList(deckOne.cards.toList, deckOneUserTwoId)
      )

      cardIdOpt_U1D3C1 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList.head).transact[IO])
      cardId_U1D3C1 <- IO.fromOption(cardIdOpt_U1D3C1)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C2 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(1)).transact[IO])
      cardId_U1D3C2 <- IO.fromOption(cardIdOpt_U1D3C2)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C3 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(2)).transact[IO])
      cardId_U1D3C3 <- IO.fromOption(cardIdOpt_U1D3C3)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C4 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(3)).transact[IO])
      cardId_U1D3C4 <- IO.fromOption(cardIdOpt_U1D3C4)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C5 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(4)).transact[IO])
      cardId_U1D3C5 <- IO.fromOption(cardIdOpt_U1D3C5)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C6 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(5)).transact[IO])
      cardId_U1D3C6 <- IO.fromOption(cardIdOpt_U1D3C6)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C7 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(6)).transact[IO])
      cardId_U1D3C7 <- IO.fromOption(cardIdOpt_U1D3C7)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C8 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(7)).transact[IO])
      cardId_U1D3C8 <- IO.fromOption(cardIdOpt_U1D3C8)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C9 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(8)).transact[IO])
      cardId_U1D3C9 <- IO.fromOption(cardIdOpt_U1D3C9)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C10 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(9)).transact[IO])
      cardId_U1D3C10 <- IO.fromOption(cardIdOpt_U1D3C10)(throw new Exception("Card id found."))

      cardIdOpt_U1D1C1 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList.head).transact[IO])
      cardId_U1D1C1 <- IO.fromOption(cardIdOpt_U1D1C1)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C2 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(1)).transact[IO])
      _ <- IO.fromOption(cardIdOpt_U1D1C2)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C3 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(2)).transact[IO])
      cardId_U1D1C3 <- IO.fromOption(cardIdOpt_U1D1C3)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C4 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(3)).transact[IO])
      cardId_U1D1C4 <- IO.fromOption(cardIdOpt_U1D1C4)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C5 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(4)).transact[IO])
      cardId_U1D1C5 <- IO.fromOption(cardIdOpt_U1D1C5)(throw new Exception("Card id found."))

      cardIdOpt_U1D2C2 <- DbManager
        .transactor.use(readCardIdByDeckIdAndContent(deckTwoUserOneId, deckTwo.cards.toList(1)).transact[IO])
      cardId_U1D2C2 <- IO.fromOption(cardIdOpt_U1D2C2)(throw new Exception("Card id found."))

      _ <- DbManager.transactorBlock(createAnswerInfoTable *>
        createAnswerInfo(AnswerInfo(Good, 20), cardId_U1D2C2) *>

        createAnswerInfo(AnswerInfo(Good, 5), cardId_U1D3C5) *>
        createAnswerInfo(AnswerInfo(Hard, 20), cardId_U1D3C10) *> createAnswerInfo(AnswerInfo(Good, 70), cardId_U1D3C10) *>
        createAnswerInfo(AnswerInfo(Easy, 20), cardId_U1D3C7) *>
        createAnswerInfo(AnswerInfo(Fail, 20), cardId_U1D3C3) *>
        createAnswerInfo(AnswerInfo(Easy, 20), cardId_U1D3C6) *> createAnswerInfo(AnswerInfo(Fail, 20), cardId_U1D3C6) *>
        createAnswerInfo(AnswerInfo(Good, 20), cardId_U1D3C2) *> createAnswerInfo(AnswerInfo(Hard, 20), cardId_U1D3C2) *>
        createAnswerInfo(AnswerInfo(Hard, 20), cardId_U1D3C8) *>
        createAnswerInfo(AnswerInfo(Fail, 20), cardId_U1D3C2) *>
        createAnswerInfo(AnswerInfo(Fail, 20), cardId_U1D3C1) *> createAnswerInfo(AnswerInfo(Easy, 25), cardId_U1D3C1) *>
        createAnswerInfo(AnswerInfo(Easy, 15), cardId_U1D3C9) *>
        createAnswerInfo(AnswerInfo(Easy, 30), cardId_U1D3C4) *>

        createAnswerInfo(AnswerInfo(Good, 35), cardId_U1D1C3) *>
        createAnswerInfo(AnswerInfo(Fail, 20), cardId_U1D1C1) *>
        createAnswerInfo(AnswerInfo(Hard, 20), cardId_U1D1C5) *>
        createAnswerInfo(AnswerInfo(Easy, 65), cardId_U1D1C4)
      )

      deckInfoOpt <- DbManager.transactor.use(readEarliestFreshDeckInfo(userOne.name).transact[IO])
      deckInfo <- IO.fromOption(deckInfoOpt)(throw new Exception("Deck info not found."))
      cardInfoInsufficientList <- DbManager.transactor.use(
        readCardInfoListWithInsufficientAnswer(List(deckOneUserOneId, deckThreeUserOneId), SelectionSize).transact[IO])
      cardInfoSlowestList <- DbManager.transactor.use(
        readCardInfoWithSlowestSufficientAnswer(List(deckOneUserOneId, deckThreeUserOneId), SelectionSize).transact[IO])

      _ <- DbManager.transactor.use(dropTable("answer_info").transact[IO])
      _ <- DbManager.transactor.use(dropTable("card").transact[IO])
      _ <- DbManager.transactor.use(dropTable("deck").transact[IO])
      _ <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      _ <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
      _ <- DbManager.transactor.use(dropType("rate_enum").transact[IO])
    } yield {
      assert(deckInfo._1 == deckTwoUserOneId)
      assert(deckInfo._2 == deckTwo.description)
      assert(cardInfoInsufficientList.length == SelectionSize)
      assert(cardInfoInsufficientList.head._1 == cardId_U1D1C1)
      assert(cardInfoSlowestList.length == SelectionSize)
      assert(cardInfoSlowestList.last._1 == cardId_U1D3C10)
    }
  }
}