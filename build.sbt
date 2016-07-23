

name := "onlineNewsMachineLearning"

version := "1.0"

scalaVersion := "2.11.7"

scapegoatVersion := "1.1.0"

val betterFilesVersion = "2.16.0"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.1",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.typelevel" %% "cats" % "0.4.0",
  "com.github.pathikrit" %% "better-files" % betterFilesVersion
)

scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint" // recommended additional warnings
)

fork in run := true
connectInput in run := true   // this is needed because by default, the standard input of the sbt process is not forwarded to the forked process
coverageEnabled := true
