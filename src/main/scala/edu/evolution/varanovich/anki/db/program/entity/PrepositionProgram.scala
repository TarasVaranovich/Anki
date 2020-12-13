package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.adt.PartOfSpeech.Preposition
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object PrepositionProgram {
  val createPrepositionTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE preposition(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxRusWordLength) NOT NULL,
         |transcription VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  val createPreposition: Preposition => ConnectionIO[Int] = (preposition: Preposition) => {
    val query: String =
      s"""INSERT INTO preposition(
         |value,
         |translation,
         |transcription) VALUES (
         |'${preposition.value}',
         |'${preposition.translation}',
         |'${preposition.transcription}');""".stripMargin
    Fragment.const(query).update.run
  }

  val createPrepositionListSafely: (List[Preposition]) => ConnectionIO[Int] = (prepositions: List[Preposition]) => {
    val query: String =
      s"""INSERT INTO
         |preposition( value, translation, transcription)
         |VALUES (?,?,?) ON CONFLICT DO NOTHING;""".stripMargin
    Update[Preposition](query).updateMany(prepositions)
  }

  val readPreposition: String => ConnectionIO[Option[Preposition]] = (value: String) => {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription
         |FROM preposition WHERE value = '$value'""".stripMargin
    Fragment.const(query).query[Preposition].option
  }

  val readPrepositionById: Int => ConnectionIO[Option[Preposition]] = (id: Int) => {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription
         |FROM preposition WHERE id = $id""".stripMargin
    Fragment.const(query).query[Preposition].option
  }

  val readAllPrepositions: ConnectionIO[List[Preposition]] = {
    val query: String = s"SELECT value, translation, transcription FROM preposition"
    Fragment.const(query).query[Preposition].to[List]
  }

  val updatePreposition: Preposition => ConnectionIO[Int] = (preposition: Preposition) => {
    val query: String =
      s"""UPDATE preposition SET
         |translation = '${preposition.translation}',
         |transcription = '${preposition.transcription}'
       WHERE value = '${preposition.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  val deletePreposition: Preposition => ConnectionIO[Int] = (preposition: Preposition) => {
    val query: String = s"DELETE FROM preposition WHERE value = '${preposition.value}'"
    Fragment.const(query).update.run
  }
}
