package gutta.apievolution.benchmarks;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.math3.distribution.NormalDistribution;

import static org.apache.commons.math3.stat.StatUtils.*;

class WriteAggregatedDataProcessor extends AbstractBenchmarkResultProcessor {

	private static final Locale OUTPUT_LOCALE = Locale.US;
	
	private static final double CONFIDENCE_LEVEL = 0.99;
	
	private static final double ALPHA = 1.0 - CONFIDENCE_LEVEL;
	
	private static final double BLOCK_SIZE = 1000;
	
	private final Writer outputWriter;
	
	private final Map<ExperimentKey, List<Integer>> durations = new LinkedHashMap<>();
	
	public WriteAggregatedDataProcessor(Writer outputWriter) {
		this.outputWriter = outputWriter;
	}
	
	@Override
	protected void processData(String data) {
		var fields = data.split(";");
		if (fields.length != 6) {
			return;
		}
		
		var benchmarkName = fields[0];
		var experimentSize = Integer.parseInt(fields[1]);
		var durationMs = Integer.valueOf(fields[3]);
		
		var experimentKey = new ExperimentKey(benchmarkName, experimentSize);
		
		var durationsPerExperiment = this.durations.computeIfAbsent(experimentKey, key -> new ArrayList<>());
		durationsPerExperiment.add(durationMs);
	}
		
	@Override
	public void atEnd() {
		this.durations.forEach(this::writeResultsForExperiment);
	}
	
	private void writeResultsForExperiment(ExperimentKey key, List<Integer> durationsMs) {
		var durationsMsDouble = new double[durationsMs.size()];
		
		for (var index = 0; index < durationsMs.size(); index++) {
			durationsMsDouble[index] = durationsMs.get(index).doubleValue();
		}
		
		// Determine average and standard deviation
		var averageDurationMs = mean(durationsMsDouble);
		var stdDevDurationMs = Math.sqrt(variance(durationsMsDouble, averageDurationMs));
		
		// Calculate the width of the confidence interval
		var normalDistribution = new NormalDistribution();
		var criticalValue = normalDistribution.inverseCumulativeProbability(1.0 - (ALPHA / 2));
		var marginOfError = criticalValue * (stdDevDurationMs / Math.sqrt(BLOCK_SIZE));			
				
		try {
			this.outputWriter.write(key.name());
			this.outputWriter.write(';');
			this.outputWriter.write(String.valueOf(key.experimentSize()));
			this.outputWriter.write(';');
			this.outputWriter.write(String.format(OUTPUT_LOCALE, "%.06f", averageDurationMs));
			this.outputWriter.write(';');
			this.outputWriter.write(String.format(OUTPUT_LOCALE, "%.06f", stdDevDurationMs));
			this.outputWriter.write(';');
			this.outputWriter.write(String.format(OUTPUT_LOCALE, "%.06f", marginOfError));
			this.outputWriter.write("\n");
		} catch (IOException e) {
			throw new ResultProcessingException("Error writing aggregated result data.", e);
		}
	}
	
	private record ExperimentKey(String name, int experimentSize) {};

}
