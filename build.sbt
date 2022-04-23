lazy val root = (project in file("."))
  .settings(
    name         := "scala-rampart",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "3.1.2",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-freespec"       % "3.2.11" % Test,
      "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.11" % Test
    )
  )
