package gutta.apievolution.jmh.inprocess.objectmapping;

import org.junit.jupiter.api.Test;

class ObjectMappingConversionBenchmarksTest {

    /**
     * Make sure that the benchmark methods are invokable without errors.
     */
    @Test
    void invocationTests() {
        ObjectMappingConversionBenchmarks benchmarks = new ObjectMappingConversionBenchmarks();
        
        benchmarks.invokeEmptyResult_short();
        benchmarks.invokeResult010_short();
        benchmarks.invokeAndInspectResult010_short();
        benchmarks.invokeResult025_short();
        benchmarks.invokeAndInspectResult025_short();
        benchmarks.invokeResult050_short();
        benchmarks.invokeAndInspectResult050_short();
        benchmarks.invokeResult075_short();
        benchmarks.invokeAndInspectResult075_short();
        benchmarks.invokeResult100_short();
        benchmarks.invokeAndInspectResult100_short();
        benchmarks.invokeResult250_long();
        benchmarks.invokeAndInspectResult250_long();
        benchmarks.invokeResult500_long();
        benchmarks.invokeAndInspectResult500_long();
    }

}
