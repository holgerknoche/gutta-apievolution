package gutta.apievolution.benchmarks;

abstract class AbstractBenchmarkResultProcessor implements BenchmarkResultProcessor {

	private static final String DATA_LINE_PREFIX = "# ";
	
	private static boolean isDataLine(String line) {
		return line.startsWith(DATA_LINE_PREFIX);
	}
	
	private static String dataPartOfLine(String line) {
		return line.substring(DATA_LINE_PREFIX.length());
	}
	
	@Override
	public final void processLine(String line) {
		if (isDataLine(line)) {
			this.processData(dataPartOfLine(line));
		}		
	}
	
	protected abstract void processData(String data);
	
}
