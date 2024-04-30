# gutta-apievolution
![build status](https://github.com/holgerknoche/gutta-apievolution/actions/workflows/maven-build.yml/badge.svg)

Core libraries of the GUTTA API evolution approach presented in our paper "Continuous API Evolution in Heterogenous Enterprise Software Systems" (https://doi.org/10.1109/ICSA51549.2021.00014).

Please note that this is work-in-progress, as we are currently improving, documenting and consolidating our initial prototype.

# Components
This project currently consists of the following components:

1. The core library `gutta-apievolution-core`, which contains the basic elements and algorithms for API our evolution approach,
1. the component `gutta-apievolution-customerexample`, which implements the customer example from the paper using several technologies,
1. the domain-specific language for specifying API definitions, contained in `gutta-apievolution-dsl`,
1. a library `gutta-apievolution-fixedformat`, which provides support for fixed-format communication such as employed in COBOL,
1. a library `gutta-apievolution-inprocess`, which implements our approach for in-process communication by means of dynamic proxies and object mapping,
1. a Maven plugin `gutta-apievolution-java-codegen-plugin`, which illustrates how code for the internal representation can be generated,
1. `gutta-apievolution-json`, which shows how the approach can be applied to JSON-based communication,
1. `gutta-apievolution-inprocess`, which shows how the approach can be applied to in-process invocations in Java
1. `gutta-apievolution-fixedformat`, which shows how the approach can be applied to fixed-format communication such as used by in-process COBOL communication,
1. `gutta-apievolution-repository`, a simple repository implementation built with Quarkus,
1. and JMH benchmarks for performance impact evaluation in `gutta-apievolution-jmh`,
1. and several tools in the component `gutta-apievolution-tools`.

Furthermore, a proof-of-concept for COBOL is contained in the `gutta-apievolution-cobol` directory.

# Building Podman Images
For our performance benchmarks, we use Podman images to facilitate running the benchmarks on different machines.
The steps for building these images are described below.

## Building an Image for Java (optional)
The JMH benchmarks require a Java VM.
This requirement may be satisfied by either using an available image that already contains a Java VM, or by creating one.
The latter is useful if a particular Java version or base image is desired.
A sample `Containerfile` for such an image is contained in the `gutta-apievolution-jmh/podman/java`, and can be used as follows to build an image:

1. Download a JDK, such as Temurin
1. Navigate to `gutta-apievolution-jmh/podman/java`
1. Extract the JDK.
This usually creates a new directory, e.g., `jdk-21.0.1+12`
1. Build the image, passing the version part of the directory name as the build argument `jdkVersion` (i.e., `21.0.1+12` for the directory `jdk-21.0.1+12`).
It is also useful to immediately tag the image.
The following command builds the image and tags it as `java:21`:
`podman build -t java:21 --build-arg jdkVersion=21.0.1+12 .`

## Building an Image for the JMH Benchmarks
The following steps are required for building the image for the JMH benchmarks:

1. Build the Java components by running `./mvnw install` in the root directory
1. Copy the file `gutta-apievolution-jmh/target/gutta-apievolution-jmh*.jar` to `gutta-apievolution-jmh/podman/gutta-jmh-benchmarks`
1. Navigate to `gutta-apievolution-jmh/podman/gutta-jmh-benchmarks`
1. Build the image by running `podman build -t gutta-jmh-benchmarks:latest .`

## Building an Image for the COBOL Benchmarks
The following steps are required for building the image for the COBOL benchmarks:

1. Install GNU COBOL (usually by installing the package `gnucobol`).
1. Navigate to `gutta-apievolution-cobol`
1. Build the COBOL components by running `./build.sh`
1. Copy the following files to `gutta-apievolution-cobol/podman/dist`: `APICONV`, `*.dat`, and `*.so`
1. Navigate to `gutta-apievolution-cobol/podman`
1. Build the image by running `podman build -t gutta-cobol-benchmarks:latest .`

# Running the Benchmarks
The benchmarks can be run by simply running the respective images.
Both images provide parameters for finer control over the benchmark execution, which are described below.
All parameters are environment variables, which can be passed to the container by the `-e` option.

## Parameters of the JMH Benchmarks Image
The JMH benchmarks image provides the following parameters:

- `FORKS`: The number of JVM forks that JMH will perform (default: `3`)
- `WARMUP_ITERATIONS`: The number of warmup iterations that JMH will perform for each benchmark (default: `30`).
  Note that the number of iterations required is mostly determined by the "large" benchmarks (500 fields), and may need to be considerably larger for stable results on slow machines like a Raspberry Pi.
- `ITERATIONS`: The number of timed iterations that JMH will perform for each benchmark
- `FORMAT`: The output format that JMH will use (default: `csv`)
- `BENCHMARKS`: A regular expression selecting the names of the benchmarks JMH will perform (default: `.*` for all benchmarks).
  Note that the benchmark names have size suffixes (such as `short`).
   
## Parameters of the COBOL Benchmarks Image
The COBOL benchmarks image provides the following parameters:

- `FORKS`: The number of times the benchmarks will be run
