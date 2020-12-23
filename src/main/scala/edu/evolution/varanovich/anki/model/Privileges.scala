package edu.evolution.varanovich.anki.model

import doobie.Meta
import doobie.postgres.implicits.pgEnumStringOpt
import enumeratum.EnumEntry.Lowercase
import enumeratum.{Enum, EnumEntry}

sealed trait Privileges extends EnumEntry with Lowercase
object Privileges extends Enum[Privileges] {
  override def values: IndexedSeq[Privileges] = findValues
  case object Admin extends Privileges
  case object Member extends Privileges

  private def toEnum(privileges: Privileges): String =
    privileges match {
      case Admin => "Admin"
      case Member => "Member"
    }

  private def fromEnum(str: String): Option[Privileges] =
    Option(str) collect {
      case "Admin" => Admin
      case "Member" => Member
    }

  implicit val PrivilegesMeta: Meta[Privileges] =
    pgEnumStringOpt("privileges_enum", Privileges.fromEnum, Privileges.toEnum)
}