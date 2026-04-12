package gutta.apievolution.benchmarks;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.junit.jupiter.api.Test;

/**
 * Test cases for the class {@link BenchmarkResultConverter}.
 */
class BenchmarkResultConverterTest {
	
	/**
	 * Test case: Benchmark output is converted to CSV.
	 */
	@Test
	void resultConversionToCsv() throws IOException {
		try (var writer = new BufferedWriter(new FileWriter(new File("test.csv")))) {
			var resultProcessor = new WriteAggregatedDataProcessor(writer);
			
			new BenchmarkResultConverter().convertResult(new File("results-pi.txt"), resultProcessor);
		}
	}

}
