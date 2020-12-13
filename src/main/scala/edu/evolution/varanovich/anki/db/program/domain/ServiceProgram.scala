package edu.evolution.varanovich.anki.db.program.domain

import doobie.{ConnectionIO, Fragment}

object ServiceProgram {
  val readMaxId: String => ConnectionIO[Option[Int]] = (name: String) => {
    val query: String = s"SELECT MAX(id) from $name"
    Fragment.const(query).query[Int].option
  }

  val readRowsCount: String => ConnectionIO[Option[Int]] = (name: String) => {
    val query: String = s"SELECT COUNT(*) from $name"
    Fragment.const(query).query[Int].option
  }

  val dropTable: String => ConnectionIO[Int] = (name: String) => {
    val query: String = s"DROP TABLE IF EXISTS $name"
    Fragment.const(query).update.run
  }

  val dropType: String => ConnectionIO[Int] = (name: String) => {
    val query: String = s"DROP TYPE IF EXISTS $name"
    Fragment.const(query).update.run
  }
}