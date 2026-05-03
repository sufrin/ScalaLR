ThisBuild / scalaVersion := "2.13.15"
ThisBuild / crossPaths := false
ThisBuild / organization := "org.sufrin"
//ThisBuild / name := "scalalr"
ThisBuild / version := "0.8.0"
ThisBuild / libraryDependencies +=
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation"
)

lazy val root = (project in file("."))
  .aggregate(bootstrap, shared, stage1, stage1a, utilities, testbed)
  .settings(
    publish / skip := true,
    name := "scalalr",
    //idePackagePrefix := Some("org.sufrin.scalalr")
  )

//
lazy val commandlinemodules = (project in file("commandlinemodules"))
  .aggregate(bootstrap, shared, stage1, utilities, testbed)   // exclude slab
  .settings(
    publish / skip := true
  )

lazy val loggingApi = (project in file("logging-api"))

lazy val shared = (project in file("shared"))
  .dependsOn(utilities, loggingApi)

lazy val bootstrap = (project in file("bootstrap"))
  .dependsOn(shared, utilities)

lazy val stage1 = (project in file("stage1")) // first language atop bootstrap
  .dependsOn(shared, utilities, bootstrap)

lazy val stage1a = (project in file("stage1a")) // second language atop bootstrap
  .dependsOn(shared, utilities, bootstrap)

lazy val stage2 = (project in file("stage2")) // 
  .dependsOn(shared, utilities)

lazy val utilities = (project in file("utilities"))
  .dependsOn(loggingApi)

lazy val testbed =  (project in file("testbed"))
  .dependsOn(shared, utilities, bootstrap)
