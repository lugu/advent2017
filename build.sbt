import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "advent",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Advent2017",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += scalaTest % Test
  )
