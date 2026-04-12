package gutta.apievolution.benchmarks.inprocess.objectmapping;

import org.junit.jupiter.api.Test;

class ObjectMappingConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     */
    @Test
    void invocationTests() {
        ObjectMappingConversionBenchmarks benchmarks = new ObjectMappingConversionBenchmarks();
        
        benchmarks.invokeEmptyResult();
        benchmarks.invokeResult010();
        benchmarks.invokeAndInspectResult010();
        benchmarks.invokeResult025();
        benchmarks.invokeAndInspectResult025();
        benchmarks.invokeResult050();
        benchmarks.invokeAndInspectResult050();
        benchmarks.invokeResult075();
        benchmarks.invokeAndInspectResult075();
        benchmarks.invokeResult100();
        benchmarks.invokeAndInspectResult100();
        benchmarks.invokeResult250();
        benchmarks.invokeAndInspectResult250();
        benchmarks.invokeResult500();
        benchmarks.invokeAndInspectResult500();
    }

}
