# Automated Quality-Driven Security Risk Identification in Cyber-Physical Production Systems

This prototype identifies security risk sources (i.e., threats and vulnerabilities) and types of attack consequences based on AutomationML (AML) artifacts.
The results of the risk identification process can be used to identify cascading effects and consequences of attacks affecting product quality.

## Installation

1. Build AML2OWL

This prototype depends on a forked version of the [implementation of the bidirectional translation between AML and OWL](https://github.com/sbaresearch/ETFA2019) for the ETFA 2019 paper ["Interpreting OWL Complex Classes in AutomationML based on Bidirectional Translation"](https://arxiv.org/abs/1906.04240) by Hua and Hein.
Clone the [repository](https://github.com/sbaresearch/ETFA2019), compile the projects, and assemble an application bundle of `aml_owl`:
```
$ cd aml_models
$ mvn clean compile install
$ cd ../aml_io
$ mvn clean compile install
$ cd ../aml_owl
$ mvn clean compile install assembly:single
```

2. Setup the AMLsec Base Directory

Clone this repository, create the application base directory (usually located in the user's home directory), and place the files located in [amlsec-base-dir](https://github.com/qualsec-paper/amlsec/blob/master/amlsec-base-dir) and the assembled AML2OWL JAR (located in `aml_owl/target/`) there.
The AMLsec base directory and the path to the AML2OWL JAR must be set in the [configuration file](https://github.com/qualsec-paper/amlsec/blob/master/amlsec/src/main/resources/application.conf) using the keys `baseDir` and `amlToOwlProgram`, respectively.

3. Setup Apache Jena Fuseki

Install and start [Apache Jena Fuseki](https://jena.apache.org/documentation/fuseki2/):
```
$ java -jar <path_to_apache-jena-fuseki-X.Y.Z>/fuseki-server.jar --update
```

4. Build AMLsec

Finally, build and start the app by using [sbt](https://www.scala-sbt.org/).
```
$ sbt "runMain org.sba_research.worker.Main -q"
```

## Usage

The implemented method utilizes a semantic information mapping mechanism realized by means of AML libraries.
These [AML security extension libraries](https://github.com/qualsec-paper/amlsec/tree/master/aml-libs/amlsec) and [AML quality extension libraries](https://github.com/qualsec-paper/amlsec/tree/master/aml-libs/amlqual) can be easily reused in engineering projects by importing them into AML files.

The capabilities of this prototype are demonstrated in a [case study](https://github.com/qualsec-paper/amlsec/blob/master/amlsec-base-dir/quality-case-study/A/CaseStudy_A.aml).
Running this prototype as is will yield the knowledge base (can be accessed via Fuseki), which also includes the results of the risk identification process, and the results of the case study.

### Cluster

The prototype utilizes the [Akka](https://akka.io/) framework and is able to distribute the risk identification workload among multiple nodes. The [Akka distributed workers sample](https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala) was used as a template.

To run the cluster with multiple nodes:

1. Start Cassandra:
```
$ sbt "runMain org.sba_research.worker.Main cassandra -q"
```

2. Start the first seed node:
```
$ sbt "runMain org.sba_research.worker.Main 2551 -q"
```

3. Start a front-end node:
```
$ sbt "runMain org.sba_research.worker.Main 3001 -q"
```

4. Start a worker node (the second parameter denotes the number of worker actors, e.g., 3):
```
$ sbt "runMain org.sba_research.worker.Main 5001 3 -q"
```

If you run the nodes on separate machines, you will have to adapt the Akka settings in the [configuration file](https://github.com/qualsec-paper/amlsec/blob/master/amlsec/src/main/resources/application.conf).

## Performance Assessment

The measurements and log files obtained during the performance assessment are available upon request.