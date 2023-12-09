package gutta.apievolution.jmh.json;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.jmh.JMHBenchmarkTemplate;
import gutta.apievolution.jmh.json.consumer.ConsumerParameter;
import gutta.apievolution.jmh.json.consumer.TestMethodEmptyConsumerProxy;
import gutta.apievolution.jmh.json.provider.TestMethodEmptyProviderProxy;
import gutta.apievolution.json.ProviderOperationProxy;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

/**
 * JMH benchmarks to determine the performance impact of format conversion in a JSON-based setting.
 */
public class JSONConversionBenchmarks extends JMHBenchmarkTemplate {

    private static final JsonRequestRouter ROUTER = createRouter();
    
    private static final ConsumerParameter CONSUMER_PARAMETER = new ConsumerParameter();
    
    private static JsonRequestRouter createRouter() {
        ProviderOperationProxy<?, ?>[] proxies = new ProviderOperationProxy<?, ?>[] {
            new TestMethodEmptyProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS)
        };
        
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, CONSUMER_API_DEFINITION);       
        return new JsonRequestRouter(resolution, proxies);
    }
    
    private static final TestMethodEmptyConsumerProxy TEST_METHOD_EMPTY_PROXY = new TestMethodEmptyConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);
    
    // TODO Add benchmarks just for JSON processing for comparison
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeEmptyResult() {
        TEST_METHOD_EMPTY_PROXY.invoke(CONSUMER_PARAMETER);
    }

}
