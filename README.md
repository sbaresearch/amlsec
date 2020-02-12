# Automated Security Risk Identification Based on Engineering Data

This prototype identifies security risk sources (i.e., threats and vulnerabilities) and types of attack consequences based on AutomationML (AML) artifacts.
The results of the risk identification process can be used to generate cyber-physical attack graphs, which model multistage cyber attacks that potentially lead to physical damage.

## Installation

This prototype depends on a forked version of the [implementation of the bidirectional translation between AML and OWL](https://github.com/sbaresearch/ETFA2019) for the ETFA 2019 paper ["Interpreting OWL Complex Classes in AutomationML based on Bidirectional Translation"](https://arxiv.org/abs/1906.04240) by Hua and Hein.
Clone the aforementioned repository, compile the projects, and run the AML2OWL application to transform your AMLsec-augmented plant know-how (i.e., AML artifact) to OWL.

After that, convert the generated OWL file (RDF/XML syntax) to the Turtle syntax (e.g., by using [Protégé](https://protege.stanford.edu/)).

Then, clone this repository, place this file in the app's `resources` directory, and adapt the AML file name (`aml.fileName`) in the application [configuration file}(https://github.com/sbaresearch/amlsec/blob/master/amlsec/src/main/resources/application.conf). 

Finally, start the app by using [sbt](https://www.scala-sbt.org/).

## Usage

The implemented method utilizes a semantic information mapping mechanism realized by means of AML libraries.
These [AML security extension libraries](https://github.com/sbaresearch/amlsec/tree/master/amlsec-libs) (named AMLsec) can be easily reused in engineering projects by importing them into AML files.

The capabilities of this prototype are demonstrated in a [case study](https://github.com/sbaresearch/amlsec/blob/master/case-study/CaseStudy.aml).
Running this prototype as is will yield the [knowledge base](https://github.com/sbaresearch/amlsec/blob/master/amlsec/src/main/resources/amlsec.ttl), which also includes the results of the risk identification process, and the following pruned cyber-physical attack graph:

![Cyber-Physical Attack Graph](https://github.com/sbaresearch/amlsec/blob/master/case-study/pruned_ag.svg?sanitize=true)