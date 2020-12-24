package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Adjective
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object AdjectiveProgram {
  def createAdjectiveTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE adjective(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxRusWordLength) NOT NULL,
         |transcription VARCHAR($MaxEngWordLength),
         |comparative VARCHAR($MaxEngWordLength),
         |superlative VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  def createAdjective(adjective: Adjective): ConnectionIO[Int] = {
    val query: String =
      s"""INSERT INTO adjective(
         |value,
         |translation,
         |transcription,
         |comparative,
         |superlative) VALUES (
         |'${adjective.value}',
         |'${adjective.translation}',
         |'${adjective.transcription}',
         |'${adjective.comparative}',
         |'${adjective.superlative}');""".stripMargin
    Fragment.const(query).update.run
  }

  def createAdjectiveListSafely(adjectives: List[Adjective]): ConnectionIO[Int] = {
    val query: String =
      s"""INSERT INTO adjective(
         |value,
         |translation,
         |transcription,
         |comparative,
         |superlative) VALUES (?,?,?,?,?)
         |ON CONFLICT DO NOTHING;""".stripMargin
    Update[Adjective](query).updateMany(adjectives)
  }

  def readAdjective(value: String): ConnectionIO[Option[Adjective]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |comparative,
         |superlative
         |FROM adjective WHERE value = '$value'""".stripMargin
    Fragment.const(query).query[Adjective].option
  }

  def readAdjectiveById(id: Int): ConnectionIO[Option[Adjective]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |comparative,
         |superlative
         |FROM adjective WHERE id = $id""".stripMargin
    Fragment.const(query).query[Adjective].option
  }

  def readAllAdjectives: ConnectionIO[List[Adjective]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |comparative,
         |superlative
         |FROM adjective""".stripMargin
    Fragment.const(query).query[Adjective].to[List]
  }

  def updateAdjective(adjective: Adjective): ConnectionIO[Int] = {
    val query: String =
      s"""UPDATE adjective SET
         |translation = '${adjective.translation}',
         |transcription = '${adjective.transcription}',
         |comparative = '${adjective.comparative}',
         |superlative = '${adjective.superlative}'
       WHERE value = '${adjective.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  def deleteAdjective(adjective: Adjective): ConnectionIO[Int] = {
    val query: String = s"DELETE FROM adjective WHERE value = '${adjective.value}'"
    Fragment.const(query).update.run
  }
}