package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment}

object ServiceProgram {
  val dropTable: String => ConnectionIO[Int] = (name: String) => {
    val query: String = s"DROP TABLE IF EXISTS $name"
    Fragment.const(query).update.run
  }

  val dropType: String => ConnectionIO[Int] = (name: String) => {
    val query: String = s"DROP TYPE IF EXISTS $name"
    Fragment.const(query).update.run
  }
}
