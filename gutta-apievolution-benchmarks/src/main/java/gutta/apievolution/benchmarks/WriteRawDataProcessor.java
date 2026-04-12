package gutta.apievolution.benchmarks;

import java.io.IOException;
import java.io.Writer;

class WriteRawDataProcessor extends AbstractBenchmarkResultProcessor {

	private final Writer outputWriter;
	
	public WriteRawDataProcessor(Writer outputWriter) {
		this.outputWriter = outputWriter;
	}
		
	@Override
	protected void processData(String data) {
		try {		
			this.outputWriter.write(data);
			this.outputWriter.write("\n");
		} catch (IOException e) {
			throw new ResultProcessingException("Error writing result data.", e);
		}		
	}

}
