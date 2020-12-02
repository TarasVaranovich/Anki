package edu.evolution.varanovich.anki.adt

trait Privileges
object Privileges {
  case object Admin extends Privileges
  case object Member extends Privileges
}
