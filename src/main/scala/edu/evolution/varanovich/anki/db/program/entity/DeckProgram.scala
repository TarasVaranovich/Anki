package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.adt.{Card, Deck}
import edu.evolution.varanovich.anki.utility.AnkiConfig.MaxDeckDescriptionLength

object DeckProgram {
  val createDeckTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE deck(
         |id SERIAL PRIMARY KEY,
         |user_id INT REFERENCES anki_user(id_sequential) ON DELETE CASCADE,
         |description VARCHAR($MaxDeckDescriptionLength) NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  val createDeck: (Deck, Int) => ConnectionIO[Int] = (deck: Deck, userId: Int) => {
    val query: String = s"INSERT INTO deck(description, user_id) VALUES ('${deck.description}', $userId);"
    Fragment.const(query).update.run
  }

  val readDeckIdByDescriptionAndUserName: (String, String) => ConnectionIO[Option[Int]] =
    (description: String, name: String) => {
      val query: String =
        s"""SELECT deck.id
           |FROM deck
           |INNER JOIN anki_user ON deck.user_id = anki_user.id_sequential
           |WHERE deck.description = '$description' and anki_user.name = '$name'""".stripMargin
      Fragment.const(query).query[Int].option
    }

  val deleteDeck: Deck => ConnectionIO[Int] = (deck: Deck) => {
    val query: String = s"DELETE FROM deck WHERE description = '${deck.description}'"
    Fragment.const(query).update.run
  }
}
