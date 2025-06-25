package gutta.apievolution.benchmarks.json;

import gutta.apievolution.benchmarks.json.JSONConversionBenchmarks;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class JSONConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() throws IOException {
        JSONConversionBenchmarks benchmarks = new JSONConversionBenchmarks();
        
        benchmarks.immediateJsonProcessing010_short();
        benchmarks.immediateJsonProcessing025_short();
        benchmarks.immediateJsonProcessing050_short();
        benchmarks.immediateJsonProcessing075_short();
        benchmarks.immediateJsonProcessing100_short();
        benchmarks.immediateJsonProcessing250_long();
        benchmarks.immediateJsonProcessing500_long();
        
        benchmarks.indirectProcessing010_short();
        benchmarks.indirectProcessing025_short();
        benchmarks.indirectProcessing050_short();
        benchmarks.indirectProcessing075_short();
        benchmarks.indirectProcessing100_short();
        benchmarks.indirectProcessing250_long();
        benchmarks.indirectProcessing500_long();
        
        benchmarks.invokeEmptyResult_short();
        benchmarks.invokeTestMethod010_short();
        benchmarks.invokeTestMethod025_short();
        benchmarks.invokeTestMethod050_short();
        benchmarks.invokeTestMethod075_short();
        benchmarks.invokeTestMethod100_short();
        benchmarks.invokeTestMethod250_long();
        benchmarks.invokeTestMethod500_long();
    }
    
}
