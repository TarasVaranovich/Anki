package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment}
import edu.evolution.varanovich.anki.adt.AnswerInfo

object AnswerInfoProgram {
  val createAnswerInfoTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TYPE rate_enum AS ENUM ('Fail', 'Hard', 'Good', 'Easy');
         |CREATE TABLE answer_info(
         |id SERIAL PRIMARY KEY,
         |card_id INT REFERENCES card(id) ON DELETE CASCADE,
         |rate rate_enum NOT NULL,
         |duration_sec SMALLINT NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  val createAnswerInfo: (AnswerInfo, Int) => ConnectionIO[Int] = (info: AnswerInfo, cardId: Int) => {
    val query: String =
      s"""INSERT INTO answer_info(
         |card_id,
         |rate,
         |duration_sec) VALUES (
         |'$cardId',
         |'${info.rate}',
         |'${info.durationSec}');""".stripMargin
    Fragment.const(query).update.run
  }

  val readAnswerInfoList: (Int) => ConnectionIO[List[AnswerInfo]] = (cardId: Int) => {
    val query: String = s"SELECT rate, duration_sec FROM answer_info WHERE card_id = '$cardId'"
    Fragment.const(query).query[AnswerInfo].to[List]
  }
}