
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
    libraryDependencies ++= commonDeps
  )
lazy val mod1 = project.dependsOn(common)
  .settings(
    libraryDependencies ++= commonDeps
  )

name := "learning scala"

lazy val dependencies = new {
  val scalaTestVer = "3.0.4"
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVer
  val xtract = "com.lucidchart" %% "xtract" % "2.0.1"
  val xml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
}

lazy val commonDeps = Seq(
  dependencies.scalaTest,
  dependencies.xtract,
  dependencies.xml
)


