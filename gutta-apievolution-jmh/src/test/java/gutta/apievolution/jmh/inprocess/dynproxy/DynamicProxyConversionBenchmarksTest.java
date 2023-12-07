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
        benchmarks.invokeResult100();
        benchmarks.invokeAndInspectResult100();
        benchmarks.invokeResult500();
        benchmarks.invokeAndInspectResult500();
    }
    
}
