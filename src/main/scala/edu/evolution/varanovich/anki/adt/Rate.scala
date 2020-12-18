package edu.evolution.varanovich.anki.adt

import doobie.Meta
import doobie.postgres.implicits.pgEnumStringOpt
import enumeratum.EnumEntry.Lowercase
import enumeratum.{Enum, EnumEntry}

sealed trait Rate extends EnumEntry with Lowercase
object Rate extends Enum[Rate] {
  override def values: IndexedSeq[Rate] = findValues
  case object Fail extends Rate
  case object Hard extends Rate
  case object Good extends Rate
  case object Easy extends Rate

  private def toEnum(rate: Rate): String =
    rate match {
      case Fail => "Fail"
      case Hard => "Hard"
      case Good => "Good"
      case Easy => "Easy"
    }

  private def fromEnum(str: String): Option[Rate] =
    Option(str) collect {
      case "Fail" => Fail
      case "Hard" => Hard
      case "Good" => Good
      case "Easy" => Easy
    }

  implicit val RateMeta: Meta[Rate] = pgEnumStringOpt("rate_enum", Rate.fromEnum, Rate.toEnum)
}