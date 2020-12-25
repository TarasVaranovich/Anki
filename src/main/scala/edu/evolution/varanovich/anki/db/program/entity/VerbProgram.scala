package edu.evolution.varanovich.anki.db.program.entity

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Verb
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object VerbProgram {
  private val insertFragment: Fragment =
    fr"""INSERT INTO verb(
        value,
        translation,
        transcription,
        third_person,
        present_participle,
        past_participle)
        VALUES (?,?,?,?,?,?)""".stripMargin

  private val selectFragment: Fragment =
    fr"""SELECT
        value,
        translation,
        transcription,
        third_person,
        present_participle,
        past_participle
        FROM verb""".stripMargin

  def createVerbTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE verb(
         id SERIAL PRIMARY KEY,
         value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         translation VARCHAR($MaxRusWordLength) NOT NULL,
         transcription VARCHAR($MaxEngWordLength),
         third_person VARCHAR($MaxEngWordLength),
         present_participle VARCHAR($MaxEngWordLength),
         past_participle VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  def createVerb(verb: Verb): ConnectionIO[Int] = Update[Verb](insertFragment.query.sql).run(verb)

  def createVerbListSafely(verbs: List[Verb]): ConnectionIO[Int] = {
    Update[Verb]((insertFragment ++ fr"ON CONFLICT DO NOTHING;").query.sql).updateMany(verbs)
  }

  def readVerb(value: String): ConnectionIO[Option[Verb]] =
    (selectFragment ++ fr"WHERE value = $value").query[Verb].option

  def readVerbById(id: Int): ConnectionIO[Option[Verb]] = (selectFragment ++ fr"WHERE id = $id").query[Verb].option

  def readAllVerbs: ConnectionIO[List[Verb]] = selectFragment.query[Verb].to[List]

  def updateVerb(verb: Verb): ConnectionIO[Int] =
    fr"""UPDATE verb SET
        translation = ${verb.translation},
        transcription = ${verb.transcription},
        third_person = ${verb.thirdPerson},
        present_participle = ${verb.presentParticiple},
        past_participle = ${verb.pastParticiple}
       WHERE value = ${verb.value}""".stripMargin
      .update.run

  def deleteVerb(verb: Verb): ConnectionIO[Int] = fr"DELETE FROM verb WHERE value = ${verb.value}".update.run
}