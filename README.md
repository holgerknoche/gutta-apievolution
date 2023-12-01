# gutta-apievolution
![build status](https://github.com/holgerknoche/gutta-apievolution/actions/workflows/maven-build.yml/badge.svg)

Core libraries of the GUTTA API evolution approach presented in our paper "Continuous API Evolution in Heterogenous Enterprise Software Systems" (https://doi.org/10.1109/ICSA51549.2021.00014).

Please note that this is work-in-progress, as we are currently improving, documenting and consolidating our initial prototype.

# Components
This project currently consists of the following components:

1. The core library `gutta-apievolution-core`, which contains the basic elements and algorithms for API our evolution approach,
1. the domain-specific language for specifying API definitions, contained in `gutta-apievolution-dsl`,
1. a Maven plugin `gutta-apievolution-java-codegen-plugin`, which illustrates how code for the internal representation can be generated,
1. `gutta-apievolution-json`, which shows how the approach can be applied to JSON-based communication,
1. `gutta-apievolution-inprocess`, which shows how the approach can be applied to in-process invocations in Java
1. `gutta-apievolution-fixedformat`, which shows how the approach can be applied to fixed-format communication such as used by in-process COBOL communication,
1. `gutta-apievolution-repository`, a simple repository implementation built with Quarkus,
1. and JMH benchmarks for performance impact evaluation in `gutta-apievolution-jmh`.
