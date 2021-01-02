name := "Anki"

version := "0.1"

scalaVersion := "2.13.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

conflictManager := ConflictManager.default

val catsVersion = "2.2.0"
val catsTestVersion = "0.4.1"
val log4CatsVersion = "1.1.1"
val logbackVersion = "1.2.3"
val circeVersion = "0.13.0"
val http4sVersion = "0.21.7"
val doobieVersion = "0.9.4"
val scalaTestVersion = "3.3.0-SNAP2"
val enumeratumVersion = "1.6.1"
val pureconfigVersion = "0.14.0"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion % Runtime,
  "io.chrisdavenport" %% "log4cats-core" % log4CatsVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "com.codecommit" %% "cats-effect-testing-scalatest" % catsTestVersion % Test,
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "com.beachape" %% "enumeratum-circe" % enumeratumVersion,
  "com.github.pureconfig" %% "pureconfig" % pureconfigVersion
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)