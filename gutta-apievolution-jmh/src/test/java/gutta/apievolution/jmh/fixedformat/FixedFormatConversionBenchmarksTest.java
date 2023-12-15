package gutta.apievolution.jmh.fixedformat;

import org.junit.jupiter.api.Test;

class FixedFormatConversionBenchmarksTest {
    
    @Test
    void invocationTests() {
        FixedFormatConversionBenchmarks benchmarks = new FixedFormatConversionBenchmarks();
        
        benchmarks.invokeEmptyResult();
        benchmarks.resultConversionOnly10();
        benchmarks.invokeTestMethod10();
        benchmarks.resultConversionOnly25();
        benchmarks.invokeTestMethod25();
        benchmarks.resultConversionOnly50();
        benchmarks.invokeTestMethod50();
        benchmarks.resultConversionOnly75();
        benchmarks.invokeTestMethod75();
        benchmarks.resultConversionOnly100();
        benchmarks.invokeTestMethod100();
        benchmarks.resultConversionOnly250();
        benchmarks.invokeTestMethod250();
        benchmarks.resultConversionOnly500();
        benchmarks.invokeTestMethod500();
    }

}
