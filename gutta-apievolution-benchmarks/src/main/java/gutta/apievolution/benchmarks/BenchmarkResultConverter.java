package gutta.apievolution.benchmarks;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

public class BenchmarkResultConverter {	

	private static final String MODE_WRITE_RAW = "raw";
	
	private static final String MODE_WRITE_AGGREGATED = "aggregated";
	
	public static void main(String[] arguments) throws IOException {
		var configuration = parseArguments(arguments);		
		new BenchmarkResultConverter().convertResult(configuration);
	}
	
	private static ResultConverterConfiguration parseArguments(String[] arguments) {
		var argumentIndex = 0;
		String inputFileName = null;
		String outputFileName = null;
		String modeName = null;
		
		while (argumentIndex < arguments.length) {
			var currentArgument = arguments[argumentIndex];
			
			switch (currentArgument) {
			case "--input":
				inputFileName = arguments[++argumentIndex];
				break;
				
			case "--output":
				outputFileName = arguments[++argumentIndex];
				break;
				
			case "--mode":
				modeName = arguments[++argumentIndex];
				break;				
			}
			
			argumentIndex++;
		}
		
		if (inputFileName == null || outputFileName == null) {
			throw new ConfigurationException("Missing input or output file name.");
		}
		
		return new ResultConverterConfiguration(inputFileName, outputFileName, modeName);
	}	
	
	void convertResult(ResultConverterConfiguration configuration) throws IOException {
		var inputFile = new File(configuration.inputFileName());
		var outputFile = new File(configuration.outputFileName());
		
		try (var writer = new BufferedWriter(new FileWriter(outputFile))) {
			var resultProcessor = this.createProcessor(configuration, writer);
			this.convertResult(inputFile, resultProcessor);
		}
	}
	
	BenchmarkResultProcessor createProcessor(ResultConverterConfiguration converterConfiguration, Writer outputWriter) {
		var modeName = converterConfiguration.mode();
		var normalizedModeName = (modeName != null) ? modeName.toLowerCase() : "";
		
		return switch (normalizedModeName) {
		case MODE_WRITE_RAW -> new WriteRawDataProcessor(outputWriter);
		case MODE_WRITE_AGGREGATED -> new WriteAggregatedDataProcessor(outputWriter);
		default -> new WriteRawDataProcessor(outputWriter);
		};
	}
	
	void convertResult(File inputFile, BenchmarkResultProcessor resultProcessor) throws IOException {
		try (var inputReader = new FileReader(inputFile)) {			
			this.convertResult(inputReader, resultProcessor);
		}
	}
	
	void convertResult(Reader inputReader, BenchmarkResultProcessor resultProcessor) throws IOException {
		try (var reader = new BufferedReader(inputReader)) {
			resultProcessor.beforeFirstLine();
			
			while (true) {
				var currentLine = reader.readLine();
				if (currentLine == null) {
					return;
				}
		
				var parts = currentLine.split(" - ");
				if (parts.length != 2) {
					continue;
				}
				
				resultProcessor.processLine(parts[1]);
			}
		} finally {
			resultProcessor.afterLastLine();
		}
		
	}
	
	record ResultConverterConfiguration(String inputFileName, String outputFileName, String mode) {}
		
	private static class ConfigurationException extends RuntimeException {
		
		private static final long serialVersionUID = -8202272835760426332L;

		public ConfigurationException(String message) {
			super(message);
		}
		
	}
	
}
