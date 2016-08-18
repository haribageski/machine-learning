

name := "onlineNewsMachineLearning"

version := "1.0"

javaOptions += "-Xmx2G"

// increase the time between polling for file changes when using continuous execution
pollInterval := 1000

// append several options to the list of options passed to the Java compiler
javacOptions ++= Seq("-source", "2", "-target", "2")

// add a JVM option to use when forking a JVM for 'run'
javaOptions += "-Xmx2G"


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
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0" classifier "models",
  "org.scalaz" %% "scalaz-core" % "7.1.3"
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
