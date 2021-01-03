package edu.evolution.varanovich.anki.domain

final case class Partition(averageSize: Int, extendedSize: Int)
object Partition {
  def empty: Partition = Partition(0, 0)
}
