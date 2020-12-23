package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Noun
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object NounProgram {
  val createNounTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE noun(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxRusWordLength) NOT NULL,
         |transcription VARCHAR($MaxEngWordLength),
         |plural VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  val createNoun: Noun => ConnectionIO[Int] = (noun: Noun) => {
    val query: String =
      s"""INSERT INTO noun(
         |value,
         |translation,
         |transcription,
         |plural) VALUES (
         |'${noun.value}',
         |'${noun.translation}',
         |'${noun.transcription}',
         |'${noun.plural}');""".stripMargin
    Fragment.const(query).update.run
  }

  val createNounListSafely: (List[Noun]) => ConnectionIO[Int] = (nouns: List[Noun]) => {
    val query: String =
      s"""INSERT INTO noun(
         |value,
         |translation,
         |transcription,
         |plural) VALUES (?,?,?,?)
         |ON CONFLICT DO NOTHING;""".stripMargin
    Update[Noun](query).updateMany(nouns)
  }

  val readNoun: String => ConnectionIO[Option[Noun]] = (value: String) => {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |plural
         |FROM noun WHERE value = '$value'""".stripMargin
    Fragment.const(query).query[Noun].option
  }

  val readNounById: Int => ConnectionIO[Option[Noun]] = (id: Int) => {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |plural
         |FROM noun WHERE id = $id""".stripMargin
    Fragment.const(query).query[Noun].option
  }

  val readAllNouns: ConnectionIO[List[Noun]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |plural
         |FROM noun""".stripMargin
    Fragment.const(query).query[Noun].to[List]
  }

  val updateNoun: Noun => ConnectionIO[Int] = (noun: Noun) => {
    val query: String =
      s"""UPDATE noun SET
         |translation = '${noun.translation}',
         |transcription = '${noun.transcription}',
         |plural = '${noun.plural}'
       WHERE value = '${noun.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  val deleteNoun: Noun => ConnectionIO[Int] = (noun: Noun) => {
    val query: String = s"DELETE FROM noun WHERE value = '${noun.value}'"
    Fragment.const(query).update.run
  }
}