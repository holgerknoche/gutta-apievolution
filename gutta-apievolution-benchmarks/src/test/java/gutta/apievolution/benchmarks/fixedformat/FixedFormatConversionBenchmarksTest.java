package gutta.apievolution.benchmarks.fixedformat;

import org.junit.jupiter.api.Test;

class FixedFormatConversionBenchmarksTest {
    
    @Test
    void invocationTests() {
        FixedFormatConversionBenchmarks benchmarks = new FixedFormatConversionBenchmarks();
        
        benchmarks.invokeEmptyResult();
        benchmarks.resultConversionOnly010();
        benchmarks.invokeTestMethod010();
        benchmarks.resultConversionOnly025();
        benchmarks.invokeTestMethod025();
        benchmarks.resultConversionOnly050();
        benchmarks.invokeTestMethod050();
        benchmarks.resultConversionOnly075();
        benchmarks.invokeTestMethod075();
        benchmarks.resultConversionOnly100();
        benchmarks.invokeTestMethod100();
        benchmarks.resultConversionOnly250();
        benchmarks.invokeTestMethod250();
        benchmarks.resultConversionOnly500();
        benchmarks.invokeTestMethod500();
    }

}
