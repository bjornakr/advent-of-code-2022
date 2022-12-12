lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc-2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := "3.2.1",

    libraryDependencies ++= Seq(
       "org.typelevel" %% "cats-core" % "2.9.0",
       "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    )
  )
