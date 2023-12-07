package gutta.apievolution.jmh.inprocess.objectmapping;

import org.junit.jupiter.api.Test;

class ObjectMappingConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     */
    @Test
    void invocationTests() {
        ObjectMappingConversionBenchmarks benchmarks = new ObjectMappingConversionBenchmarks();
        
        benchmarks.invokeResult10();
        benchmarks.invokeAndInspectResult10();
        benchmarks.invokeResult100();
        benchmarks.invokeAndInspectResult100();
    }

}
