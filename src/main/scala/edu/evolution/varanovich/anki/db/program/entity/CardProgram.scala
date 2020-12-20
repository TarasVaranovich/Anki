package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.adt.Card
import edu.evolution.varanovich.anki.utility.AnkiConfig.MaxCardFieldLength

object CardProgram {
  val createCardTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE card(
         |id SERIAL PRIMARY KEY,
         |deck_id INT REFERENCES deck(id) ON DELETE CASCADE,
         |question VARCHAR($MaxCardFieldLength) NOT NULL,
         |answer VARCHAR($MaxCardFieldLength) NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  val createCard: (Card, Int) => ConnectionIO[Int] = (card: Card, deckId: Int) => {
    val query: String =
      s"""INSERT INTO card(
         |deck_id,
         |question,
         |answer) VALUES (
         |'$deckId',
         |'${card.question}',
         |'${card.answer}');""".stripMargin
    Fragment.const(query).update.run
  }

  val createCardList: (List[Card], Int) => ConnectionIO[Int] = (card: List[Card], deckId: Int) => {
    val query: String = s"INSERT INTO card(deck_id, question, answer) VALUES ($deckId, ?, ?);"
    Update[Card](query).updateMany(card)
  }

  val readCardIdByDeckIdAndContent: (Int, Card) => ConnectionIO[Option[Int]] = (deckId: Int, card: Card) => {
    val query: String =
      s"""SELECT id FROM card WHERE
         |deck_id = '$deckId' AND
         |question = '${card.question}' AND
         |answer = '${card.answer}'""".stripMargin
    Fragment.const(query).query[Int].option
  }

  val readCardInfoListWithInsufficientAnswer: (List[Int], Int) => ConnectionIO[List[(Int, Card)]] =
    (decks: List[Int], count: Int) => {
      val query: String =
        s"""SELECT card.id, question, answer FROM card WHERE card.id IN
           |(SELECT card_id FROM answer_info WHERE answer_info.id IN
           |(SELECT MAX(answer_info.id) FROM card
           |INNER JOIN answer_info ON card.id = answer_info.card_id
           |GROUP BY card.id) AND (rate = 'Fail' OR rate = 'Hard'))
           |AND deck_id IN (${decks.mkString(",")})
           |ORDER BY card.id ASC LIMIT $count;""".stripMargin
      Fragment.const(query).query[(Int, Card)].to[List]
    }

  val readCardInfoWithSlowestSufficientAnswer: (List[Int], Int) => ConnectionIO[List[(Int, Card)]] =
    (decks: List[Int], count: Int) => {
      val query: String =
        s"""SELECT card.id, question, answer FROM card WHERE card.id IN
           |(SELECT card_id FROM answer_info WHERE answer_info.id IN
           |(SELECT MAX(answer_info.id) FROM card
           |INNER JOIN answer_info ON card.id = answer_info.card_id
           |GROUP BY card.id) AND (rate = 'Good' OR rate = 'Easy') ORDER BY duration_sec DESC LIMIT $count)
           |AND deck_id IN (${decks.mkString(",")})
           |ORDER BY card.id;""".stripMargin
      Fragment.const(query).query[(Int, Card)].to[List]
    }

  val readCardList: Int => ConnectionIO[List[Card]] = (deckId: Int) => {
    val query: String =
      s"SELECT question, answer FROM card WHERE deck_id = '$deckId'"
    Fragment.const(query).query[Card].to[List]
  }
}