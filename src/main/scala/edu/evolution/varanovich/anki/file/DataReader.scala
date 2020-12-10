package edu.evolution.varanovich.anki.file

import cats.effect.{IO, Resource}
import cats.implicits._
import edu.evolution.varanovich.anki.file.FileAliases.stringValue
import edu.evolution.varanovich.anki.utility.WordValidator.validTranslation

import scala.util.{Failure, Success, Try}

object DataReader {
  def readFromFile[A](fileName: String, parser: List[String] => Option[A]): IO[Seq[A]] = {
    val alias = fileName.split("/").last.split("-").head
    val fileAlias = FileAliases.valueOf(alias)
    Try(scala.io.Source.fromResource(fileName)) match {
      case Success(source) => Resource
        .fromAutoCloseable(IO(source))
        .use(source => IO(source.getLines().flatMap(str => parser(extractParts(str, fileAlias))).toSeq))
      case Failure(_) => IO(Seq())
    }
  }

  def all[A](fileAlias: FileAliases, parser: List[String] => Option[A]): IO[Seq[A]] =
    first(FilesCount)(fileAlias)
      .map(path => readFromFile(path, parser))
      .sequence
      .map(list => list.reduce((prev, next) => prev ++ next))

  private def extractParts(value: String, alias: FileAliases): List[String] = {
    val cleaned = value.replace("\u00a0", "")
    val parts: List[String] = cleaned.split(",").toList.map(_.replace("\"", "").trim)
    if (alias == FileAliases.Phrase) {
      parts
    } else {
      val translation: String = parts.filter(translation => validTranslation(translation))
        .reduce((prev, next) => s"$prev, $next")
      val partOfSpeech: List[String] = parts.filter(translation => !validTranslation(translation))
      partOfSpeech :+ translation
    }
  }

  private def first(filesCount: Int)(alias: FileAliases): List[String] =
    (1 to filesCount by 1).toList.map(fileNumber => s"data/english_$fileNumber/${stringValue(alias)}-Table-1.csv")
}