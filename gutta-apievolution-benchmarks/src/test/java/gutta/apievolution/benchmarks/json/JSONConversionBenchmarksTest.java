package gutta.apievolution.benchmarks.json;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class JSONConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() throws IOException {
        JSONConversionBenchmarks benchmarks = new JSONConversionBenchmarks();
        
        benchmarks.immediateJsonProcessing010();
        benchmarks.immediateJsonProcessing025();
        benchmarks.immediateJsonProcessing050();
        benchmarks.immediateJsonProcessing075();
        benchmarks.immediateJsonProcessing100();
        benchmarks.immediateJsonProcessing250();
        benchmarks.immediateJsonProcessing500();
        
        benchmarks.indirectProcessing010();
        benchmarks.indirectProcessing025();
        benchmarks.indirectProcessing050();
        benchmarks.indirectProcessing075();
        benchmarks.indirectProcessing100();
        benchmarks.indirectProcessing250();
        benchmarks.indirectProcessing500();
        
        benchmarks.invokeEmptyResult();
        benchmarks.invokeTestMethod010();
        benchmarks.invokeTestMethod025();
        benchmarks.invokeTestMethod050();
        benchmarks.invokeTestMethod075();
        benchmarks.invokeTestMethod100();
        benchmarks.invokeTestMethod250();
        benchmarks.invokeTestMethod500();
    }
    
}
