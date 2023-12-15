package gutta.apievolution.jmh.json;

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
        
        benchmarks.immediateJsonProcessing10();
        benchmarks.immediateJsonProcessing25();
        benchmarks.immediateJsonProcessing50();
        benchmarks.immediateJsonProcessing75();
        benchmarks.immediateJsonProcessing100();
        benchmarks.immediateJsonProcessing250();
        benchmarks.immediateJsonProcessing500();
        
        benchmarks.indirectProcessing10();
        benchmarks.indirectProcessing25();
        benchmarks.indirectProcessing50();
        benchmarks.indirectProcessing75();
        benchmarks.indirectProcessing100();
        benchmarks.indirectProcessing250();
        benchmarks.indirectProcessing500();
        
        benchmarks.invokeEmptyResult();
        benchmarks.invokeTestMethod10();
        benchmarks.invokeTestMethod25();
        benchmarks.invokeTestMethod50();
        benchmarks.invokeTestMethod75();
        benchmarks.invokeTestMethod100();
        benchmarks.invokeTestMethod250();
        benchmarks.invokeTestMethod500();
    }
    
}
