package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki.model.{AnswerInfo, Rate}
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram.{dropTable, dropType}
import edu.evolution.varanovich.anki.db.program.entity.AnswerInfoProgram.{createAnswerInfo, createAnswerInfoTable, readAnswerInfoList}
import edu.evolution.varanovich.anki.db.program.entity.CardProgram.{createCardList, createCardTable, readCardIdByDeckIdAndContent}
import edu.evolution.varanovich.anki.db.program.entity.DeckProgram.{createDeck, createDeckTable, readDeckIdByDescriptionAndUserName}
import edu.evolution.varanovich.anki.db.program.entity.UserProgram.{createUser, createUserTable, readSequentialId}
import edu.evolution.varanovich.anki.utility.AnkiConfig.MaxAnswerDuration
import edu.evolution.varanovich.anki.{deckOpt, userOpt}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class AnswerInfoSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
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
}