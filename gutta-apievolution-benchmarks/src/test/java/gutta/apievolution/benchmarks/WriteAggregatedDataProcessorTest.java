package gutta.apievolution.benchmarks;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.StringWriter;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * Test cases for the class {@link WriteAggregatedDataProcessor}.
 */
class WriteAggregatedDataProcessorTest {
	
	/**
	 * Test case: Ensure that the data is aggregated as expected (separation of tests, statistical figures).
	 * 
	 * @throws IOException For convenience
	 */
	@Test
	void aggregateData() throws IOException {
		var inputLines = List.of(
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth001;1;1000;89;88.129;46.255810002192874",
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth001;1;2000;90;89.13199999999995;15.562865378161483",
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth001;1;3000;88;88.02400000000007;59.012726813170616",
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth010;10;1000;479;477.443;66.16644731092761",
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth010;10;2000;483;481.959;99.18325784948611",
				"# DefinitionResolutionBenchmarks.resolveHistoryDepth010;10;3000;476;475.481;82.44936346956563"
		);
		
		var actualOutput = this.runProcessorOn(inputLines);		
		
		var expectedOutput = """
DefinitionResolutionBenchmarks.resolveHistoryDepth001;1;89.000000;1.000000;1.487156
DefinitionResolutionBenchmarks.resolveHistoryDepth010;10;479.333333;3.511885;5.222719
""";
		
		assertEquals(expectedOutput, actualOutput);
	}
	
	/**
	 * Test case: Ensure that obviously invalid lines are ignored. 
	 * 
	 * @throws IOException For convenience
	 */
	@Test
	void invalidLinesAreIgnored() throws IOException {
		var inputLines = List.of("", "a;b;c;d;e;f;g;h", "# a;b;c;d;e;f;g;h");		
		
		var output = this.runProcessorOn(inputLines);		
		
		assertEquals("", output);
	}
	
	private String runProcessorOn(List<String> lines) throws IOException {
		try (var writer = new StringWriter()) {
			var processor = new WriteAggregatedDataProcessor(writer);
			
			lines.forEach(processor::processLine);
			processor.afterLastLine();
			
			return writer.getBuffer().toString();
		}
	}

}
