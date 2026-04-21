package gutta.apievolution.benchmarks;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * Test cases for the class {@link BenchmarkResultConverter}.
 */
class BenchmarkResultConverterTest {

	/**
	 * Test case: The appropriate lines from the result data are passed to the processor.
	 * 
	 * @throws IOException For convenience
	 */
	@Test
	void dataProcessing() throws IOException {
		var processor = new LoggingResultProcessor();
		var converter = new BenchmarkResultConverter();
	
		var inputLines = """
+ cd /opt/gutta
+ bash ./run-benchmarks.sh 200000 100000
[main] INFO gutta.apievolution.benchmarks.BenchmarkRunner - Running benchmark 'DefinitionResolutionBenchmarks.resolveHistoryDepth001' with 200000 warmup iterations and 100000 timed iterations.
[main] INFO gutta.apievolution.benchmarks.BenchmarkRunner - Warmup iteration 1000 completed.
[main] INFO gutta.apievolution.benchmarks.BenchmarkRunner - Warmup iteration 2000 completed.
[main] INFO gutta.apievolution.benchmarks.BenchmarkRunner - Timed iteration 1000 completed, last block took 89 ms. Average 88.129 mus, std. dev. 46.255810002192874 mus.
[main] INFO gutta.apievolution.benchmarks.BenchmarkRunner - # DefinitionResolutionBenchmarks.resolveHistoryDepth001;1;1000;89;88.129;46.255810002192874
""";
		
		try (var reader = new StringReader(inputLines)) {
			converter.convertResult(reader, processor);
		}
		
		var expectedLines = List.of(
				"Running benchmark 'DefinitionResolutionBenchmarks.resolveHistoryDepth001' with 200000 warmup iterations and 100000 timed iterations.",
				"Warmup iteration 1000 completed.",
				"Warmup iteration 2000 completed.",
				"Timed iteration 1000 completed, last block took 89 ms. Average 88.129 mus, std. dev. 46.255810002192874 mus.",
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth001;1;1000;89;88.129;46.255810002192874"
				);
		
		var actualLines = processor.loggedLines();
		
		
		assertEquals(expectedLines, actualLines);
	}
	
	/**
	 * Special processor that logs all processed lines for later retrieval.
	 */
	private static class LoggingResultProcessor implements BenchmarkResultProcessor {
		
		private final List<String> loggedLines = new ArrayList<>();
		
		@Override
		public void processLine(String line) {
			this.loggedLines.add(line);
		}
		
		public List<String> loggedLines() {
			return loggedLines;
		}
		
	}
	
}
