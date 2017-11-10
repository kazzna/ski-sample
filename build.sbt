name := "ski"
version := "0.1.0"
scalaVersion := "2.12.4"

javacOptions ++= Seq(
  "-source",
  "1.8",
  "-target",
  "1.8"
)

scalacOptions += "-feature"

lazy val root = (project in file("."))

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.16",
  // "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
