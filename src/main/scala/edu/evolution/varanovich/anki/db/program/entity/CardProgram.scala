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

  val readCardList: Int => ConnectionIO[List[Card]] = (deckId: Int) => {
    val query: String =
      s"SELECT question, answer FROM card WHERE deck_id = '$deckId'"
    Fragment.const(query).query[Card].to[List]
  }
}