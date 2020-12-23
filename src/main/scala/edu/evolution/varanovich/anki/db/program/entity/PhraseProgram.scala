package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Phrase
import edu.evolution.varanovich.anki.utility.VocabularyConfig.MaxPhraseLength

object PhraseProgram {
  val createPhraseTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE phrase(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxPhraseLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxPhraseLength) NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  val createPhrase: Phrase => ConnectionIO[Int] = (phrase: Phrase) => {
    val query: String =
      s"""INSERT INTO phrase(
         |value,
         |translation) VALUES (
         |'${phrase.value}',
         |'${phrase.translation}');""".stripMargin
    Fragment.const(query).update.run
  }

  val createPhraseListSafely: (List[Phrase]) => ConnectionIO[Int] = (phrases: List[Phrase]) => {
    val query: String = s"INSERT INTO phrase (value,translation) VALUES (?,?) ON CONFLICT DO NOTHING;"
    Update[Phrase](query).updateMany(phrases)
  }

  val readPhrase: String => ConnectionIO[Option[Phrase]] = (value: String) => {
    val query: String =
      s"""SELECT
         |value,
         |translation
         |FROM phrase WHERE value = '$value'""".stripMargin
    Fragment.const(query).query[Phrase].option
  }

  val readPhraseById: Int => ConnectionIO[Option[Phrase]] = (id: Int) => {
    val query: String =
      s"""SELECT
         |value,
         |translation
         |FROM phrase WHERE id = $id""".stripMargin
    Fragment.const(query).query[Phrase].option
  }

  val readAllPhrases: ConnectionIO[List[Phrase]] = {
    val query: String = s"SELECT value, translation FROM phrase"
    Fragment.const(query).query[Phrase].to[List]
  }

  val updatePhrase: Phrase => ConnectionIO[Int] = (phrase: Phrase) => {
    val query: String =
      s"""UPDATE phrase SET
         |translation = '${phrase.translation}'
       WHERE value = '${phrase.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  val deletePhrase: Phrase => ConnectionIO[Int] = (phrase: Phrase) => {
    val query: String = s"DELETE FROM phrase WHERE value = '${phrase.value}'"
    Fragment.const(query).update.run
  }
}
