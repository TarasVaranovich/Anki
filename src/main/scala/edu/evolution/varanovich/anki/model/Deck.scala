package edu.evolution.varanovich.anki.model

final case class Deck private(cards: Set[Card], description: String) {
  override def equals(that: Any): Boolean = that match {
    case that: Deck => (cards == that.cards) && (description.equalsIgnoreCase(that.description))
    case _ => false
  }
}
object Deck {
  def from(cards: Set[Card], description: String): Option[Deck] = {
    if (cards.nonEmpty && description.nonEmpty && !description.isBlank)
      Some(Deck(cards, description)) else None
  }
}