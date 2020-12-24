package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Preposition
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object PrepositionProgram {
  def createPrepositionTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE preposition(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxRusWordLength) NOT NULL,
         |transcription VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  def createPreposition(preposition: Preposition): ConnectionIO[Int] = {
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

  def createPrepositionListSafely(prepositions: List[Preposition]): ConnectionIO[Int] = {
    val query: String =
      s"""INSERT INTO
         |preposition( value, translation, transcription)
         |VALUES (?,?,?) ON CONFLICT DO NOTHING;""".stripMargin
    Update[Preposition](query).updateMany(prepositions)
  }

  def readPreposition(value: String): ConnectionIO[Option[Preposition]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription
         |FROM preposition WHERE value = '$value'""".stripMargin
    Fragment.const(query).query[Preposition].option
  }

  def readPrepositionById(id: Int): ConnectionIO[Option[Preposition]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription
         |FROM preposition WHERE id = $id""".stripMargin
    Fragment.const(query).query[Preposition].option
  }

  def readAllPrepositions: ConnectionIO[List[Preposition]] = {
    val query: String = s"SELECT value, translation, transcription FROM preposition"
    Fragment.const(query).query[Preposition].to[List]
  }

  def updatePreposition(preposition: Preposition): ConnectionIO[Int] = {
    val query: String =
      s"""UPDATE preposition SET
         |translation = '${preposition.translation}',
         |transcription = '${preposition.transcription}'
       WHERE value = '${preposition.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  def deletePreposition(preposition: Preposition): ConnectionIO[Int] = {
    val query: String = s"DELETE FROM preposition WHERE value = '${preposition.value}'"
    Fragment.const(query).update.run
  }
}