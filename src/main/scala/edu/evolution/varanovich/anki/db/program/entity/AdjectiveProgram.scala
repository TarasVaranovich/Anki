package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.adt.PartOfSpeech.Adjective
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object AdjectiveProgram {
  val createAdjectiveTable: ConnectionIO[Int] = {
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

  val createAdjective: Adjective => ConnectionIO[Int] = (adjective: Adjective) => {
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

  val createAdjectiveListSafely: (List[Adjective]) => ConnectionIO[Int] = (adjectives: List[Adjective]) => {
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

  val readAdjective: String => ConnectionIO[Option[Adjective]] = (value: String) => {
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

  val readAdjectiveById: Int => ConnectionIO[Option[Adjective]] = (id: Int) => {
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

  val readAllAdjectives: ConnectionIO[List[Adjective]] = {
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

  val updateAdjective: Adjective => ConnectionIO[Int] = (adjective: Adjective) => {
    val query: String =
      s"""UPDATE adjective SET
         |translation = '${adjective.translation}',
         |transcription = '${adjective.transcription}',
         |comparative = '${adjective.comparative}',
         |superlative = '${adjective.superlative}'
       WHERE value = '${adjective.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  val deleteAdjective: Adjective => ConnectionIO[Int] = (adjective: Adjective) => {
    val query: String = s"DELETE FROM adjective WHERE value = '${adjective.value}'"
    Fragment.const(query).update.run
  }
}
