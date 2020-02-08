name := "amlsec"

version := "0.1"

scalaVersion := "2.13.1"

val jenaVersion = "3.9.0"

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.apache.jena" % "jena-core" % jenaVersion,
  "org.apache.jena" % "jena-arq" % jenaVersion,
  "com.typesafe" % "config" % "1.3.2",
  "org.slf4j" % "slf4j-simple" % "1.6.4",
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "org.apache.commons" % "commons-lang3" % "3.8.1",
  "org.topbraid" % "shacl" % "1.3.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "guru.nidi" % "graphviz-java" % "0.12.1"
)

mainClass in assembly := Some("org.amlsec.MainApp")