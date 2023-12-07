package gutta.apievolution.jmh.inprocess.dynproxy;

import org.junit.jupiter.api.Test;

class DynamicProxyConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     */
    @Test
    void invocationTests() {
        DynamicProxyConversionBenchmarks benchmarks = new DynamicProxyConversionBenchmarks();
        
        benchmarks.invokeResult10();
        benchmarks.invokeAndInspectResult10();
    }
    
}
