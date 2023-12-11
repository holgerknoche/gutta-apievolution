package gutta.apievolution.jmh.fixedformat;

import org.junit.jupiter.api.Test;

class FixedFormatConversionBenchmarksTest {
    
    @Test
    void invocationTests() {
        FixedFormatConversionBenchmarks benchmarks = new FixedFormatConversionBenchmarks();
        
        benchmarks.invokeTestMethod100();
    }

}
