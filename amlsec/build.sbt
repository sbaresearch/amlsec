name := "amlsec"

version := "0.1"

scalaVersion := "2.13.1"

enablePlugins(ScalaxbPlugin)

val jenaVersion = "3.16.0"
val akkaVersion = "2.6.6"
val cassandraPluginVersion = "0.103"

scalaxbPackageName in(Compile, scalaxb) := "generated"
scalaxbAutoPackages in(Compile, scalaxb) := true
sourceManaged in(Compile, scalaxb) := (sourceDirectory in Compile).value / "scala/sbt-scalaxb"
scalaxbProtocolPackageName in(Compile, scalaxb) := Some("xmlprotocol")

Global / cancelable := true

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.apache.jena" % "jena-core" % jenaVersion,
  "org.apache.jena" % "jena-arq" % jenaVersion,
  "org.apache.jena" % "jena-fuseki-main" % jenaVersion,
  "com.typesafe" % "config" % "1.3.2",
  "org.slf4j" % "slf4j-simple" % "1.6.4",
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "org.apache.commons" % "commons-lang3" % "3.8.1",
  "org.topbraid" % "shacl" % "1.3.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "guru.nidi" % "graphviz-java" % "0.12.1",
  // AKKA
  "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % "10.2.0-M1",
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-query" % akkaVersion,
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion, // this is probably not necessary
  "com.typesafe.akka" %% "akka-persistence-cassandra" % cassandraPluginVersion,
  // this allows us to start cassandra from the sample
  "com.typesafe.akka" %% "akka-persistence-cassandra-launcher" % cassandraPluginVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  // XML
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M1",
  // https://mvnrepository.com/artifact/org.glassfish.jaxb/jaxb-runtime
  "org.glassfish.jaxb" % "jaxb-runtime" % "2.3.3",
  // test dependencies
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "commons-io" % "commons-io" % "2.4" % Test
)

// mainClass in assembly := Some("org.sba_research.AMLsecApp")

// cf. https://github.com/sbt/sbt/issues/3963
// cf. https://stackoverflow.com/a/55545558/5107545
run := Defaults.runTask(fullClasspath in Runtime, mainClass in run in Compile, runner in run).evaluated