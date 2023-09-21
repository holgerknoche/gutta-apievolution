package gutta.apievolution.jmh;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

/**
 * JMH benchmarks to determine the performance impact of format conversion in a JSON-based setting.
 */
public class JSONConversionBenchmarks {
	
	@Benchmark
	@BenchmarkMode(Mode.AverageTime)
	@OutputTimeUnit(TimeUnit.MICROSECONDS)
	public void someBenchmark() {
		// Nothing to do as of now
	}

}
