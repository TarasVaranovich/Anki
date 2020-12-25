package edu.evolution.varanovich.anki.db.program.entity

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Phrase
import edu.evolution.varanovich.anki.utility.VocabularyConfig.MaxPhraseLength

object PhraseProgram {
  private val insertFragment: Fragment = fr"INSERT INTO phrase (value,translation) VALUES (?,?)"
  private val selectFragment: Fragment = fr"SELECT value, translation FROM phrase"

  def createPhraseTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE phrase(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxPhraseLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxPhraseLength) NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  def createPhrase(phrase: Phrase): ConnectionIO[Int] = Update[Phrase](insertFragment.query.sql).run(phrase)

  def createPhraseListSafely(phrases: List[Phrase]): ConnectionIO[Int] =
    Update[Phrase]((insertFragment ++ fr"ON CONFLICT DO NOTHING;").query.sql).updateMany(phrases)

  def readPhrase(value: String): ConnectionIO[Option[Phrase]] =
    (selectFragment ++ fr"WHERE value = $value").query[Phrase].option

  def readPhraseById(id: Int): ConnectionIO[Option[Phrase]] =
    (selectFragment ++ fr"WHERE id = $id").query[Phrase].option

  def readAllPhrases: ConnectionIO[List[Phrase]] = selectFragment.query[Phrase].to[List]

  def updatePhrase(phrase: Phrase): ConnectionIO[Int] =
    fr"UPDATE phrase SET translation = ${phrase.translation} WHERE value = ${phrase.value}".update.run

  def deletePhrase(phrase: Phrase): ConnectionIO[Int] = fr"DELETE FROM phrase WHERE value = ${phrase.value}".update.run
}