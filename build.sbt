name := "Anki"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.3.0-SNAP2"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)