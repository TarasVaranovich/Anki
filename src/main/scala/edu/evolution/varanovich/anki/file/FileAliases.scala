package edu.evolution.varanovich.anki.file

trait FileAliases
object FileAliases {
  case object Adjective extends FileAliases
  case object Noun extends FileAliases
  case object Phrase extends FileAliases
  case object Preposition extends FileAliases
  case object Verb extends FileAliases

  def stringValue(fileName: FileAliases): String = {
    fileName.toString.toLowerCase
  }

  def valueOf(value: String): FileAliases = value match {
    case "adjective" => Adjective
    case "noun" => Noun
    case "phrase" => Phrase
    case "preposition" => Preposition
    case "verb" => Verb
  }
}
