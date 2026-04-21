package gutta.apievolution.benchmarks;

interface BenchmarkResultProcessor {
	
	void processLine(String line);
	
	default void beforeFirstLine() {
		// Do nothing by default
	}
	
	default void afterLastLine() {
		// Do nothing by default
	}

}
