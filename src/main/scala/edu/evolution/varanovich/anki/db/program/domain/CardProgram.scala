package edu.evolution.varanovich.anki.db.program.domain

import cats.data._
import cats.implicits._
import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment, Fragments, Update}
import edu.evolution.varanovich.anki.model.Rate.{Easy, Fail, Good, Hard}
import edu.evolution.varanovich.anki.model.{Card, Rate}
import edu.evolution.varanovich.anki.utility.AnkiConfig.MaxCardFieldLength

object CardProgram {
  private val insertFragment: Fragment = fr"INSERT INTO card(deck_id, question, answer) VALUES (?, ?, ?);"

  def createCardTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE card(
         |id SERIAL PRIMARY KEY,
         |deck_id INT REFERENCES deck(id) ON DELETE CASCADE,
         |question VARCHAR($MaxCardFieldLength) NOT NULL,
         |answer VARCHAR($MaxCardFieldLength) NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  def createCard(card: Card, deckId: Int): ConnectionIO[Int] =
    Update[(Int, Card)](insertFragment.query.sql).run((deckId, card))

  def createCardList(card: List[Card], deckId: Int): ConnectionIO[Int] =
    Update[(Int, Card)](insertFragment.query.sql).updateMany(card.map(card => (deckId, card)))

  def readCardIdByDeckIdAndContent(deckId: Int, card: Card): ConnectionIO[Option[Int]] =
    (fr"SELECT id FROM card" ++
      Fragments.whereAnd(fr"deck_id = $deckId", fr"question = ${card.question}", fr"answer = ${card.answer}"))
      .query[Int].option

  def readCardInfoListWithInsufficientAnswer(decks: List[Int], count: Int): ConnectionIO[List[(Int, Card)]] =
    readCardInfoList(decks, (Fail, Hard), Fragment.empty, fr"ASC LIMIT $count").query[(Int, Card)].to[List]

  def readCardInfoWithSlowestSufficientAnswer(decks: List[Int], count: Int): ConnectionIO[List[(Int, Card)]] =
    readCardInfoList(decks, (Good, Easy), fr"ORDER BY duration_sec DESC LIMIT $count")
      .query[(Int, Card)].to[List]

  def readCardList(deckId: Int): ConnectionIO[List[Card]] =
    fr"SELECT question, answer FROM card WHERE deck_id = $deckId".query[Card].to[List]

  private def readCardInfoList(decks: List[Int],
                               ratePair: (Rate, Rate),
                               answerInfoSelectionFilter: Fragment = Fragment.empty,
                               cardInfoFilter: Fragment = Fragment.empty): Fragment = {
    val lastCardAnswerFragment: Fragment =
      fr"SELECT MAX(answer_info.id) FROM card INNER JOIN answer_info ON card.id = answer_info.card_id GROUP BY card.id"

    fr"SELECT card.id, question, answer FROM card WHERE card.id IN" ++ Fragments.and(
      Fragments.parentheses(
        fr"SELECT card_id FROM answer_info WHERE answer_info.id IN" ++
          Fragments.and(lastCardAnswerFragment,
            Fragments.or(fr"rate = ${ratePair._1}", fr"rate = ${ratePair._2}")) ++
          answerInfoSelectionFilter
      ),
      Fragments.in(fr"deck_id", NonEmptyList.apply(0, decks))
    ) ++ fr"ORDER BY card.id" ++ cardInfoFilter
  }
}