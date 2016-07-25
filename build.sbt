

name := "onlineNewsMachineLearning"

version := "1.0"

scalaVersion := "2.11.7"

scapegoatVersion := "1.1.0"

val betterFilesVersion = "2.16.0"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  // https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
  "joda-time" % "joda-time" % "2.9.1",
//  "onlinenewsmachinelearning" % "onlinenewsmachinelearning_2.11" % "1.0"  exclude("org.slf4j", "slf4j-jdk14"),
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.typelevel" %% "cats" % "0.4.0",
  "com.github.pathikrit" %% "better-files" % betterFilesVersion,
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0" classifier "models"
).map(_.exclude("ch.qos.logback", "logback-classic"))

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
