package edu.evolution.varanovich.anki.db.program.entity

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Noun
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object NounProgram {
  private val insertFragment: Fragment =
    fr"INSERT INTO noun(value, translation, transcription, plural) VALUES (?,?,?,?)"
  private val selectFragment: Fragment = fr"SELECT value, translation, transcription, plural FROM noun"

  def createNounTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE noun(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxRusWordLength) NOT NULL,
         |transcription VARCHAR($MaxEngWordLength),
         |plural VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  def createNoun(noun: Noun): ConnectionIO[Int] = Update[Noun](insertFragment.query.sql).run(noun)

  def createNounListSafely(nouns: List[Noun]): ConnectionIO[Int] =
    Update[Noun]((insertFragment ++ fr"ON CONFLICT DO NOTHING;").query.sql).updateMany(nouns)

  def readNoun(value: String): ConnectionIO[Option[Noun]] =
    (selectFragment ++ fr"WHERE value = $value").query[Noun].option

  def readNounById(id: Int): ConnectionIO[Option[Noun]] = (selectFragment ++ fr"WHERE id = $id").query[Noun].option

  def readAllNouns: ConnectionIO[List[Noun]] = selectFragment.query[Noun].to[List]

  def updateNoun(noun: Noun): ConnectionIO[Int] =
    fr"""UPDATE noun SET
        |translation = ${noun.translation},
        |transcription = ${noun.transcription},
        |plural = ${noun.plural}
       WHERE value = ${noun.value}""".stripMargin
      .update.run

  def deleteNoun(noun: Noun): ConnectionIO[Int] =
    fr"DELETE FROM noun WHERE value = ${noun.value}".update.run
}