val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent Of Code",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.specs2" %% "specs2-core" % "5.0.0-RC-22" % Test
  )
