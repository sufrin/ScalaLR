ThisBuild / scalaVersion := "2.13.15"
ThisBuild / crossPaths := false
ThisBuild / organization := "org.sufrin"
ThisBuild / name := "scalalr"
ThisBuild / version := "0.8.0"
ThisBuild / libraryDependencies +=
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0"

lazy val root = (project in file("."))
  .aggregate(bootstrap, shared, scalalr, utilities)
  .settings(
    publish / skip := true,
    name := "scalalr",
    idePackagePrefix := Some("org.sufrin.scalalr")
  )

lazy val loggingApi = (project in file("logging-api"))

lazy val shared = (project in file("shared"))
  .dependsOn(utilities, loggingApi)

lazy val bootstrap = (project in file("bootstrap"))
  .dependsOn(shared, utilities)

lazy val scalalr = (project in file("scalalr"))
  .dependsOn(shared, utilities)

lazy val utilities = (project in file("utilities"))
  .dependsOn(loggingApi)
