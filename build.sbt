ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val dependencies =
  new {
    val zioVersion = "1.0.12"
    val zioHttpVersion = "1.0.0.0-RC23"
    val akkaVersion = "2.6.18"


    val akkaStreams = "com.typesafe.akka" %% "akka-stream" % akkaVersion
    val zio = "dev.zio" %% "zio" % "1.0.12"
    val zioTest = "dev.zio" %% "zio-test" % zioVersion % Test
    val zioHttp = "io.d11" %% "zhttp" % zioHttpVersion
  }

lazy val root = (project in file("."))
  .settings(
    name := "Swoosh",
    libraryDependencies ++= Seq(
      dependencies.zio,
      dependencies.zioTest,
      dependencies.zioHttp,
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
