package edu.evolution.varanovich.anki.domain

sealed trait Alias {
  def name: String
}
object Alias {
  case object Adjective extends Alias {
    override def name: String = "adjective"
  }
  case object Noun extends Alias {
    override def name: String = "noun"
  }
  case object Phrase extends Alias {
    override def name: String = "phrase"
  }
  case object Preposition extends Alias {
    override def name: String = "preposition"
  }
  case object Verb extends Alias {
    override def name: String = "verb"
  }
}