val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent Of Code",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.specs2" %% "specs2-core" % "5.4.1" % Test
  )
