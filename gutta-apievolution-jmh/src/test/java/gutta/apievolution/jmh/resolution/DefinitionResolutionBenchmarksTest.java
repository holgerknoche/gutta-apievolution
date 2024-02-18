package gutta.apievolution.jmh.resolution;

import org.junit.jupiter.api.Test;

class DefinitionResolutionBenchmarksTest {
    
    @Test
    void benchmarkInvocationTests() {
        DefinitionResolutionBenchmarks benchmarks = new DefinitionResolutionBenchmarks();
        
        benchmarks.resolveHistoryDepth001();
        benchmarks.resolveHistoryDepth010();
        benchmarks.resolveHistoryDepth020();
        benchmarks.resolveHistoryDepth030();
        benchmarks.resolveHistoryDepth040();
        benchmarks.resolveHistoryDepth050();
        benchmarks.resolveHistoryDepth060();
        benchmarks.resolveHistoryDepth070();
        benchmarks.resolveHistoryDepth080();
        benchmarks.resolveHistoryDepth090();
        benchmarks.resolveHistoryDepth100();
    }

}
