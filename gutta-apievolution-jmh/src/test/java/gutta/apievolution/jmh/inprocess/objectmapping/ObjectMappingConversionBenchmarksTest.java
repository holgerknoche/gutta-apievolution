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
        benchmarks.invokeResult25();
        benchmarks.invokeAndInspectResult25();
        benchmarks.invokeResult50();
        benchmarks.invokeAndInspectResult50();
        benchmarks.invokeResult75();
        benchmarks.invokeAndInspectResult75();
        benchmarks.invokeResult100();
        benchmarks.invokeAndInspectResult100();
        benchmarks.invokeResult250();
        benchmarks.invokeAndInspectResult250();
        benchmarks.invokeResult500();
        benchmarks.invokeAndInspectResult500();
    }

}
