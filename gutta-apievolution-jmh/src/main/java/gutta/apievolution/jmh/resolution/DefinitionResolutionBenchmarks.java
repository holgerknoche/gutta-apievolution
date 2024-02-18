package gutta.apievolution.jmh.resolution;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * JMH benchmarks to determine the scaling of definition resolution for larger and larger histories.
 */
public class DefinitionResolutionBenchmarks {
    
    private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath("apis/customer-consumer-v1.api", "test.provider", 0);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_1 = ProviderApiLoader.loadHistoryFromClasspath("apis/customer-provider-revision-1.api");
    
    private static final Set<Integer> SUPPORTED_REVISIONS_1 = range(0, 1);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_10 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 10));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_10 = range(0, 10);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_20 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 20));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_20 = range(0, 20);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_30 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 30));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_30 = range(0, 30);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_40 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 40));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_40 = range(0, 40);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_50 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 50));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_50 = range(0, 50);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_60 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 60));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_60 = range(0, 60);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_70 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 70));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_70 = range(0, 70);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_80 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 80));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_80 = range(0, 80);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_90 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 90));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_90 = range(0, 90);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY_100 = ProviderApiLoader.loadHistoryFromClasspath(multiply("apis/customer-provider-revision-1.api", 100));
    
    private static final Set<Integer> SUPPORTED_REVISIONS_100 = range(0, 100);
    
    private static String[] multiply(String value, int times) {
        String[] array = new String[times];
        
        for (int index = 0; index < times; index++) {
            array[index] = value;
        }
        
        return array;
    }
    
    private static Set<Integer> range(int begin, int end) {
        Set<Integer> values = new HashSet<>(end - begin);
        
        for (int value = begin; value < end; value++) {
            values.add(value);
        }
        
        return values;
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth001() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_1, SUPPORTED_REVISIONS_1, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth010() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_10, SUPPORTED_REVISIONS_10, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth020() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_20, SUPPORTED_REVISIONS_20, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth030() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_30, SUPPORTED_REVISIONS_30, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth040() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_40, SUPPORTED_REVISIONS_40, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth050() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_50, SUPPORTED_REVISIONS_50, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth060() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_60, SUPPORTED_REVISIONS_60, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth070() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_70, SUPPORTED_REVISIONS_70, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth080() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_80, SUPPORTED_REVISIONS_80, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth090() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_90, SUPPORTED_REVISIONS_90, CONSUMER_API);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resolveHistoryDepth100() {
        new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY_100, SUPPORTED_REVISIONS_100, CONSUMER_API);
    }

}
