package edu.evolution.varanovich.anki.adt

import doobie.Meta
import doobie.postgres.implicits.pgEnumStringOpt

trait Privileges
object Privileges {
  case object Admin extends Privileges
  case object Member extends Privileges

  def toEnum(privileges: Privileges): String =
    privileges match {
      case Admin => "Admin"
      case Member => "Member"
    }

  def fromEnum(str: String): Option[Privileges] =
    Option(str) collect {
      case "Admin" => Admin
      case "Member" => Member
    }

  def valueOf(value: String): Option[Privileges] = value match {
    case "admin" => Some(Admin)
    case "member" => Some(Member)
    case _ => None
  }

  def parseUnsafe(value: String): Privileges = value match {
    case "admin" => Admin
    case "member" => Member
    case _ => throw new Exception(s"Not valid privileges string '$value'")
  }

  implicit val PrivilegesMeta: Meta[Privileges] =
    pgEnumStringOpt("privileges", Privileges.fromEnum, Privileges.toEnum)
}
