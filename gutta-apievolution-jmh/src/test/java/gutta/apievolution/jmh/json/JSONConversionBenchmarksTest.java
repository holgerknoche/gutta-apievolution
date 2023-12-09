package gutta.apievolution.jmh.json;

import org.junit.jupiter.api.Test;

class JSONConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     */
    @Test
    void invocationTests() {
        JSONConversionBenchmarks benchmarks = new JSONConversionBenchmarks();
        
        benchmarks.invokeEmptyResult();
        benchmarks.invokeTestMethod100();
    }
    
}
