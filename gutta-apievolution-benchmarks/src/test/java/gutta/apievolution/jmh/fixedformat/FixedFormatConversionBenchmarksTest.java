package gutta.apievolution.jmh.fixedformat;

import org.junit.jupiter.api.Test;

class FixedFormatConversionBenchmarksTest {
    
    @Test
    void invocationTests() {
        FixedFormatConversionBenchmarks benchmarks = new FixedFormatConversionBenchmarks();
        
        benchmarks.invokeEmptyResult_short();
        benchmarks.resultConversionOnly010_short();
        benchmarks.invokeTestMethod010_short();
        benchmarks.resultConversionOnly025_short();
        benchmarks.invokeTestMethod025_short();
        benchmarks.resultConversionOnly050_short();
        benchmarks.invokeTestMethod050_short();
        benchmarks.resultConversionOnly075_short();
        benchmarks.invokeTestMethod075_short();
        benchmarks.resultConversionOnly100_short();
        benchmarks.invokeTestMethod100_short();
        benchmarks.resultConversionOnly250_long();
        benchmarks.invokeTestMethod250_long();
        benchmarks.resultConversionOnly500_long();
        benchmarks.invokeTestMethod500_long();
    }

}
