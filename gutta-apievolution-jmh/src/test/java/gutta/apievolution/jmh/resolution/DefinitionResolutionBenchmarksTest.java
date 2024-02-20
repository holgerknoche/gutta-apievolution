package gutta.apievolution.jmh.resolution;

import org.junit.jupiter.api.Test;

class DefinitionResolutionBenchmarksTest {
    
    @Test
    void benchmarkInvocationTests() {
        DefinitionResolutionBenchmarks benchmarks = new DefinitionResolutionBenchmarks();
        
        benchmarks.resolveHistoryDepth001_short();
        benchmarks.resolveHistoryDepth010_short();
        benchmarks.resolveHistoryDepth020_short();
        benchmarks.resolveHistoryDepth030_short();
        benchmarks.resolveHistoryDepth040_short();
        benchmarks.resolveHistoryDepth050_short();
        benchmarks.resolveHistoryDepth060_short();
        benchmarks.resolveHistoryDepth070_short();
        benchmarks.resolveHistoryDepth080_short();
        benchmarks.resolveHistoryDepth090_short();
        benchmarks.resolveHistoryDepth100_short();
    }

}
