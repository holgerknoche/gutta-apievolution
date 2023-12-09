package gutta.apievolution.jmh.json;

import com.fasterxml.jackson.databind.ObjectMapper;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.jmh.JMHBenchmarkTemplate;
import gutta.apievolution.jmh.json.consumer.ConsumerParameter;
import gutta.apievolution.jmh.json.consumer.TestMethod100ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethodEmptyConsumerProxy;
import gutta.apievolution.jmh.json.provider.ProviderResult100;
import gutta.apievolution.jmh.json.provider.TestMethod100ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethodEmptyProviderProxy;
import gutta.apievolution.json.ProviderOperationProxy;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

/**
 * JMH benchmarks to determine the performance impact of format conversion in a JSON-based setting.
 */
public class JSONConversionBenchmarks extends JMHBenchmarkTemplate {

    private static final JsonRequestRouter ROUTER = createRouter();

    private static final ConsumerParameter CONSUMER_PARAMETER = new ConsumerParameter();

    private static JsonRequestRouter createRouter() {
        ProviderOperationProxy<?, ?>[] proxies = new ProviderOperationProxy<?, ?>[] {
                new TestMethodEmptyProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod100ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS) };

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS,
                CONSUMER_API_DEFINITION);
        return new JsonRequestRouter(resolution, proxies);
    }

    private static final TestMethodEmptyConsumerProxy TEST_METHOD_EMPTY_PROXY = new TestMethodEmptyConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod100ConsumerProxy TEST_METHOD_100_PROXY = new TestMethod100ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    // Objects for JSON mapping

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private static final ProviderResult100 RESULT_100 = TestMethod100ProviderProxy.createProviderResult100();

    // TODO Add benchmarks just for JSON processing for comparison

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeEmptyResult() {
        TEST_METHOD_EMPTY_PROXY.invoke(CONSUMER_PARAMETER);
    }

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void jsonProcessing100() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_100);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult100.class);
    }

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod100() {
        TEST_METHOD_100_PROXY.invoke(CONSUMER_PARAMETER);
    }

}
