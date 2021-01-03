package edu.evolution.varanovich.anki.db.program.domain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.catsSyntaxApply
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.AnswerInfoProgram._
import edu.evolution.varanovich.anki.db.program.domain.CardProgram._
import edu.evolution.varanovich.anki.db.program.domain.DeckProgram._
import edu.evolution.varanovich.anki.db.program.domain.UserProgram.{createUser, createUserTable, readSequentialId}
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram.{dropTable, dropType}
import edu.evolution.varanovich.anki.model.{AnswerInfo, Rate}
import edu.evolution.varanovich.anki.{deckOpt, deckOptSecond, deckOptThird, userOpt, userOptSecond}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import doobie.implicits._
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.model.Rate.{Easy, Fail, Good, Hard}

class AnswerInfoProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private val maxAnswerDuration = AnkiConfig.load.maxAnswerDuration
  private implicit val transactor = DbManager.transactorInstance

  "should successfully create and read answer infos" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      _ <- runTransaction(createUserTable *> createUser(user))
      deck <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      userIdOpt <- DbManager.transactorInstance.use(readSequentialId(user.name).transact[IO])
      userId <- IO.fromOption(userIdOpt)(throw new Exception("User not found"))
      _ <- runTransaction(createDeckTable *> createDeck(deck, userId))
      deckIdOpt <- DbManager.transactorInstance
        .use(readDeckIdByDescriptionAndUserName(deck.description, user.name).transact[IO])
      deckId <- IO.fromOption(deckIdOpt)(throw new Exception("Deck not found"))
      _ <- runTransaction(createCardTable *> createCardList(deck.cards.toList, deckId))

      firstCardIdOpt <- runTransaction(readCardIdByDeckIdAndContent(deckId, deck.cards.head))
      firstCardId <- IO.fromOption(firstCardIdOpt)(throw new Exception("Card id not found"))
      firstAnswerInfoOne <- IO.fromOption(
        AnswerInfo.from(Rate.Fail, maxAnswerDuration))(throw new Exception("Answer Info not created"))
      firstAnswerInfoTwo <- IO.fromOption(
        AnswerInfo.from(Rate.Good, 10))(throw new Exception("Answer Info not created"))

      lastCardIdOpt <- DbManager.transactorInstance.use(readCardIdByDeckIdAndContent(deckId, deck.cards.last).transact[IO])
      lastCardId <- IO.fromOption(lastCardIdOpt)(throw new Exception("Card id not found"))
      lastAnswerInfoOne <- IO.fromOption(
        AnswerInfo.from(Rate.Hard, 100))(throw new Exception("Answer Info not created"))
      lastAnswerInfoTwo <- IO.fromOption(
        AnswerInfo.from(Rate.Easy, 3))(throw new Exception("Answer Info not created"))

      _ <- runTransaction(createAnswerInfoTable *>
        createAnswerInfo(firstAnswerInfoOne, firstCardId) *>
        createAnswerInfo(firstAnswerInfoTwo, firstCardId) *>
        createAnswerInfo(lastAnswerInfoOne, lastCardId) *>
        createAnswerInfo(lastAnswerInfoTwo, lastCardId))

      firstAnswerInfoList <- DbManager.transactorInstance.use(readAnswerInfoList(firstCardId).transact[IO])
      lastAnswerInfoList <- DbManager.transactorInstance.use(readAnswerInfoList(lastCardId).transact[IO])

      _ <- DbManager.transactorInstance.use(dropTable("answer_info").transact[IO])
      _ <- DbManager.transactorInstance.use(dropTable("card").transact[IO])
      _ <- DbManager.transactorInstance.use(dropTable("deck").transact[IO])
      _ <- DbManager.transactorInstance.use(dropTable("anki_user").transact[IO])
      _ <- DbManager.transactorInstance.use(dropType("privileges_enum").transact[IO])
      _ <- DbManager.transactorInstance.use(dropType("rate_enum").transact[IO])
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
      _ <- runTransaction(createUserTable *> createUser(userOne) *> createUser(userTwo))
      userOneIdOpt <- DbManager.transactorInstance.use(readSequentialId(userOne.name).transact[IO])
      userOneId <- IO.fromOption(userOneIdOpt)(throw new Exception("User not found"))
      userTwoIdOpt <- DbManager.transactorInstance.use(readSequentialId(userTwo.name).transact[IO])
      userTwoId <- IO.fromOption(userTwoIdOpt)(throw new Exception("User not found"))
      deckOne <- IO.fromOption(deckOpt)(throw new Exception("Deck not created"))
      deckTwo <- IO.fromOption(deckOptSecond)(throw new Exception("Deck not created"))
      deckThree <- IO.fromOption(deckOptThird)(throw new Exception("Deck not created"))

      _ <- runTransaction(createDeckTable *>
        createDeck(deckThree, userOneId) *>
        createDeck(deckOne, userTwoId) *>
        createDeck(deckTwo, userOneId) *>
        createDeck(deckOne, userOneId)
      )

      deckOneUserOneIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckOne.description, userOne.name))
      deckOneUserOneId <- IO.fromOption(deckOneUserOneIdOpt)(throw new Exception("Deck not found"))
      deckTwoUserOneIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckTwo.description, userOne.name))
      deckTwoUserOneId <- IO.fromOption(deckTwoUserOneIdOpt)(throw new Exception("Deck not found"))
      deckThreeUserOneIdOpt <- runTransaction(readDeckIdByDescriptionAndUserName(deckThree.description, userOne.name))
      deckThreeUserOneId <- IO.fromOption(deckThreeUserOneIdOpt)(throw new Exception("Deck not found"))

      deckOneUserTwoIdOpt <- DbManager.transactorInstance
        .use(readDeckIdByDescriptionAndUserName(deckOne.description, userTwo.name).transact[IO])
      deckOneUserTwoId <- IO.fromOption(deckOneUserTwoIdOpt)(throw new Exception("Deck not found"))

      _ <- runTransaction(createCardTable *>
        createCardList(deckOne.cards.toList, deckOneUserOneId) *>
        createCardList(deckTwo.cards.toList, deckTwoUserOneId) *>
        createCardList(deckThree.cards.toList, deckThreeUserOneId) *>
        createCardList(deckOne.cards.toList, deckOneUserTwoId)
      )

      cardIdOpt_U1D3C1 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList.head).transact[IO])
      cardId_U1D3C1 <- IO.fromOption(cardIdOpt_U1D3C1)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C2 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(1)).transact[IO])
      cardId_U1D3C2 <- IO.fromOption(cardIdOpt_U1D3C2)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C3 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(2)).transact[IO])
      cardId_U1D3C3 <- IO.fromOption(cardIdOpt_U1D3C3)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C4 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(3)).transact[IO])
      cardId_U1D3C4 <- IO.fromOption(cardIdOpt_U1D3C4)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C5 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(4)).transact[IO])
      cardId_U1D3C5 <- IO.fromOption(cardIdOpt_U1D3C5)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C6 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(5)).transact[IO])
      cardId_U1D3C6 <- IO.fromOption(cardIdOpt_U1D3C6)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C7 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(6)).transact[IO])
      cardId_U1D3C7 <- IO.fromOption(cardIdOpt_U1D3C7)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C8 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(7)).transact[IO])
      cardId_U1D3C8 <- IO.fromOption(cardIdOpt_U1D3C8)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C9 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(8)).transact[IO])
      cardId_U1D3C9 <- IO.fromOption(cardIdOpt_U1D3C9)(throw new Exception("Card id found."))
      cardIdOpt_U1D3C10 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckThreeUserOneId, deckThree.cards.toList(9)).transact[IO])
      cardId_U1D3C10 <- IO.fromOption(cardIdOpt_U1D3C10)(throw new Exception("Card id found."))

      cardIdOpt_U1D1C1 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList.head).transact[IO])
      cardId_U1D1C1 <- IO.fromOption(cardIdOpt_U1D1C1)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C2 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(1)).transact[IO])
      _ <- IO.fromOption(cardIdOpt_U1D1C2)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C3 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(2)).transact[IO])
      cardId_U1D1C3 <- IO.fromOption(cardIdOpt_U1D1C3)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C4 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(3)).transact[IO])
      cardId_U1D1C4 <- IO.fromOption(cardIdOpt_U1D1C4)(throw new Exception("Card id found."))
      cardIdOpt_U1D1C5 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckOneUserOneId, deckOne.cards.toList(4)).transact[IO])
      cardId_U1D1C5 <- IO.fromOption(cardIdOpt_U1D1C5)(throw new Exception("Card id found."))

      cardIdOpt_U1D2C2 <- DbManager
        .transactorInstance.use(readCardIdByDeckIdAndContent(deckTwoUserOneId, deckTwo.cards.toList(1)).transact[IO])
      cardId_U1D2C2 <- IO.fromOption(cardIdOpt_U1D2C2)(throw new Exception("Card id found."))

      _ <- runTransaction(createAnswerInfoTable *>
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

      deckInfoOpt <- runTransaction(readEarliestFreshDeckInfo(userOne.name))
      deckInfo <- IO.fromOption(deckInfoOpt)(throw new Exception("Deck info not found."))
      cardInfoInsufficientList <- runTransaction(
        readCardInfoListWithInsufficientAnswer(List(deckOneUserOneId, deckThreeUserOneId), SelectionSize))
      cardInfoSlowestList <- runTransaction(
        readCardInfoWithSlowestSufficientAnswer(List(deckOneUserOneId, deckThreeUserOneId), SelectionSize))

      _ <- runTransaction(dropTable("answer_info"))
      _ <- runTransaction(dropTable("card"))
      _ <- runTransaction(dropTable("deck"))
      _ <- runTransaction(dropTable("anki_user"))
      _ <- runTransaction(dropType("privileges_enum"))
      _ <- runTransaction(dropType("rate_enum"))
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