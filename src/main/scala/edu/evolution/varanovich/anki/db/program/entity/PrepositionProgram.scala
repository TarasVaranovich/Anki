package edu.evolution.varanovich.anki.db.program.entity

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.config.VocabularyConfig
import edu.evolution.varanovich.anki.model.PartOfSpeech.Preposition

object PrepositionProgram {
  private val config = VocabularyConfig.load
  private val insertFragment: Fragment = fr"INSERT INTO preposition(value, translation, transcription) VALUES(?, ?, ?)"
  private val selectFragment: Fragment = fr"SELECT value, translation, transcription FROM preposition"

  def createPrepositionTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE preposition(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR(${config.maxEngWordLength}) UNIQUE NOT NULL,
         |translation VARCHAR(${config.maxRusWordLength}) NOT NULL,
         |transcription VARCHAR(${config.maxEngWordLength}));""".stripMargin
    Fragment.const(query).update.run
  }

  def createPreposition(preposition: Preposition): ConnectionIO[Int] =
    Update[Preposition](insertFragment.query.sql).run(preposition)

  def createPrepositionListSafely(prepositions: List[Preposition]): ConnectionIO[Int] =
    Update[Preposition]((insertFragment ++ fr"ON CONFLICT DO NOTHING;").query.sql).updateMany(prepositions)

  def readPreposition(value: String): ConnectionIO[Option[Preposition]] =
    (selectFragment ++ fr"WHERE value = $value").query[Preposition].option

  def readPrepositionById(id: Int): ConnectionIO[Option[Preposition]] =
    (selectFragment ++ fr"WHERE id = $id").query[Preposition].option

  def readAllPrepositions: ConnectionIO[List[Preposition]] = selectFragment.query[Preposition].to[List]

  def updatePreposition(preposition: Preposition): ConnectionIO[Int] =
    fr"""UPDATE preposition SET
        |translation = ${preposition.translation},
        |transcription = ${preposition.transcription}
       WHERE value = ${preposition.value}""".stripMargin
      .update.run

  def deletePreposition(preposition: Preposition): ConnectionIO[Int] =
    fr"DELETE FROM preposition WHERE value = ${preposition.value}".update.run
}