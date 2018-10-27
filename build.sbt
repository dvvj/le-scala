
ThisBuild / organization := "org.ditw"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.7"

lazy val global = project
  .in(file("."))
  .aggregate(
    common,
    mod1
  )

lazy val common = project
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4"
    )
  )
lazy val mod1 = project.dependsOn(common)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4"
    )
  )

name := "multi-module template"

lazy val dependencies = new {
  val scalaTestVer = "3.0.4"
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVer
}


