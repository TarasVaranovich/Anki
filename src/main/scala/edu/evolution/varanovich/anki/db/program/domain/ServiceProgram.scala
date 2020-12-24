package edu.evolution.varanovich.anki.db.program.domain

import doobie.{ConnectionIO, Fragment}

object ServiceProgram {
  def readMaxId(name: String): ConnectionIO[Option[Int]] = {
    val query: String = s"SELECT MAX(id) from $name"
    Fragment.const(query).query[Int].option
  }

  def readRowsCount(name: String): ConnectionIO[Option[Int]] = {
    val query: String = s"SELECT COUNT(*) from $name"
    Fragment.const(query).query[Int].option
  }

  def dropTable(name: String): ConnectionIO[Int] = {
    val query: String = s"DROP TABLE IF EXISTS $name"
    Fragment.const(query).update.run
  }

  def dropType(name: String): ConnectionIO[Int] = {
    val query: String = s"DROP TYPE IF EXISTS $name"
    Fragment.const(query).update.run
  }
}