package gutta.apievolution.benchmarks;

import java.io.IOException;
import java.io.StringWriter;
import java.util.List;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for the class {@link WriteRawDataProcessor}.
 */
class WriteRawDataProcessorTest {
	
	/**
	 * Test case: Raw data is written as-is with added linebreaks.
	 * 
	 * @throws IOException For convenience
	 */
	@Test
	void processRawData() throws IOException {
		var inputStrings = List.of("line1", "line2", "line3");
		
		try (var writer = new StringWriter()) {
			var processor = new WriteRawDataProcessor(writer);
			
			inputStrings.forEach(processor::processData);
			
			var expectedOutput = """
line1
line2
line3					
""";
			assertEquals(expectedOutput, writer.getBuffer().toString());
		}
		
	}

}
