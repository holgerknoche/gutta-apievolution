package gutta.apievolution.benchmarks;

interface BenchmarkResultProcessor {
	
	void processLine(String line);
	
	default void atEnd() {
		// Do nothing by default
	}

}
