package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment, Update}
import edu.evolution.varanovich.anki.model.PartOfSpeech.Verb
import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxRusWordLength}

object VerbProgram {
  val createVerbTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE verb(
         |id SERIAL PRIMARY KEY,
         |value VARCHAR($MaxEngWordLength) UNIQUE NOT NULL,
         |translation VARCHAR($MaxRusWordLength) NOT NULL,
         |transcription VARCHAR($MaxEngWordLength),
         |third_person VARCHAR($MaxEngWordLength),
         |present_participle VARCHAR($MaxEngWordLength),
         |past_participle VARCHAR($MaxEngWordLength));""".stripMargin
    Fragment.const(query).update.run
  }

  val createVerb: Verb => ConnectionIO[Int] = (verb: Verb) => {
    val query: String =
      s"""INSERT INTO verb(
         |value,
         |translation,
         |transcription,
         |third_person,
         |present_participle,
         |past_participle) VALUES (
         |'${verb.value}',
         |'${verb.translation}',
         |'${verb.transcription}',
         |'${verb.thirdPerson}',
         |'${verb.presentParticiple}',
         |'${verb.pastParticiple}');""".stripMargin
    Fragment.const(query).update.run
  }

  val createVerbListSafely: (List[Verb]) => ConnectionIO[Int] = (verbs: List[Verb]) => {
    val query: String =
      s"""INSERT INTO verb(
         |value,
         |translation,
         |transcription,
         |third_person,
         |present_participle,
         |past_participle) VALUES (?,?,?,?,?,?)
         |ON CONFLICT DO NOTHING;""".stripMargin
    Update[Verb](query).updateMany(verbs)
  }

  val readVerb: String => ConnectionIO[Option[Verb]] = (value: String) => {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |third_person,
         |present_participle,
         |past_participle
         |FROM verb WHERE value = '$value'""".stripMargin
    Fragment.const(query).query[Verb].option
  }

  val readVerbById: Int => ConnectionIO[Option[Verb]] = (id: Int) => {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |third_person,
         |present_participle,
         |past_participle
         |FROM verb WHERE id = $id""".stripMargin
    Fragment.const(query).query[Verb].option
  }

  val readAllVerbs: ConnectionIO[List[Verb]] = {
    val query: String =
      s"""SELECT
         |value,
         |translation,
         |transcription,
         |third_person,
         |present_participle,
         |past_participle
         |FROM verb""".stripMargin
    Fragment.const(query).query[Verb].to[List]
  }

  val updateVerb: Verb => ConnectionIO[Int] = (verb: Verb) => {
    val query: String =
      s"""UPDATE verb SET
         |translation = '${verb.translation}',
         |transcription = '${verb.transcription}',
         |third_person = '${verb.thirdPerson}',
         |present_participle = '${verb.presentParticiple}',
         |past_participle = '${verb.pastParticiple}'
       WHERE value = '${verb.value}'""".stripMargin
    Fragment.const(query).update.run
  }

  val deleteVerb: Verb => ConnectionIO[Int] = (verb: Verb) => {
    val query: String = s"DELETE FROM verb WHERE value = '${verb.value}'"
    Fragment.const(query).update.run
  }
}
