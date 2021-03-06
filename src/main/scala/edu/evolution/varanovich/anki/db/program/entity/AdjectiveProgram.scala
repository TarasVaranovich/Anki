package edu.evolution.varanovich.anki.db.program.entity

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.config.VocabularyConfig
import edu.evolution.varanovich.anki.model.PartOfSpeech.Adjective

object AdjectiveProgram {
  private val config = VocabularyConfig.load
  private val insertFragment: Fragment =
    fr"INSERT INTO adjective(value,translation,transcription,comparative,superlative) VALUES (?,?,?,?,?)"
  private val selectFragment: Fragment =
    fr"SELECT value, translation, transcription, comparative, superlative FROM adjective"

  def createAdjectiveTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE adjective(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR(${config.maxEngWordLength}) UNIQUE NOT NULL,
         |translation VARCHAR(${config.maxRusWordLength}) NOT NULL,
         |transcription VARCHAR(${config.maxEngWordLength}),
         |comparative VARCHAR(${config.maxEngWordLength}),
         |superlative VARCHAR(${config.maxEngWordLength}));""".stripMargin
    Fragment.const(query).update.run
  }

  def createAdjective(adjective: Adjective): ConnectionIO[Int] =
    Update[Adjective](insertFragment.query.sql).run(adjective)

  def createAdjectiveListSafely(adjectives: List[Adjective]): ConnectionIO[Int] =
    Update[Adjective]((insertFragment ++ fr"ON CONFLICT DO NOTHING;").query.sql).updateMany(adjectives)

  def readAdjective(value: String): ConnectionIO[Option[Adjective]] =
    (selectFragment ++ fr"WHERE value = $value").query[Adjective].option

  def readAdjectiveById(id: Int): ConnectionIO[Option[Adjective]] =
    (selectFragment ++ fr"WHERE id = $id").query[Adjective].option

  def readAllAdjectives: ConnectionIO[List[Adjective]] = selectFragment.query[Adjective].to[List]

  def updateAdjective(adjective: Adjective): ConnectionIO[Int] =
    fr"""UPDATE adjective SET
        |translation = ${adjective.translation},
        |transcription = ${adjective.transcription},
        |comparative = ${adjective.comparative},
        |superlative = ${adjective.superlative}
       WHERE value = ${adjective.value}""".stripMargin
      .update.run

  def deleteAdjective(adjective: Adjective): ConnectionIO[Int] =
    fr"DELETE FROM adjective WHERE value = ${adjective.value}".update.run
}