import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.brianpritchett"
ThisBuild / organizationName := "Brian Pritchett"

lazy val root = (project in file("."))
  .settings(
    name := "euchre",
    libraryDependencies ++= Seq(scalaTest % Test, "org.typelevel" %% "cats-core" % "2.1.1", "org.typelevel" %% "cats-effect" % "2.3.1")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
