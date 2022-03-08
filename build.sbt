ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val dependencies =
  new {
    val zioVersion = "2.0.0-RC2"
    val akkaVersion = "2.6.18"

    val akkaStreams = "com.typesafe.akka" %% "akka-stream" % akkaVersion
    val zio = "dev.zio" %% "zio" % zioVersion
    val zioStreams = "dev.zio" %% "zio-streams" % zioVersion
    val zioTest = "dev.zio" %% "zio-test" % zioVersion % Test
  }

lazy val root = (project in file("."))
  .settings(
    name := "Swoosh",
    libraryDependencies ++= Seq(
      dependencies.zio,
      dependencies.zioStreams,
      dependencies.zioTest,
      dependencies.akkaStreams
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    Compile / mainClass := Some("io.navidjalali.swoosh.Main"),
    assembly / mainClass := Some("io.navidjalali.swoosh.Main"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )
