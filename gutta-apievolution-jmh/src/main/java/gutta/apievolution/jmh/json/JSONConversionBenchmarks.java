package gutta.apievolution.jmh.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import gutta.apievolution.jmh.JMHBenchmarkTemplate;
import gutta.apievolution.jmh.json.consumer.ConsumerParameter;
import gutta.apievolution.jmh.json.consumer.TestMethod100ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethod10ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethod250ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethod25ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethod500ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethod50ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethod75ConsumerProxy;
import gutta.apievolution.jmh.json.consumer.TestMethodEmptyConsumerProxy;
import gutta.apievolution.jmh.json.provider.ProviderResult10;
import gutta.apievolution.jmh.json.provider.ProviderResult100;
import gutta.apievolution.jmh.json.provider.ProviderResult25;
import gutta.apievolution.jmh.json.provider.ProviderResult250;
import gutta.apievolution.jmh.json.provider.ProviderResult50;
import gutta.apievolution.jmh.json.provider.ProviderResult500;
import gutta.apievolution.jmh.json.provider.ProviderResult75;
import gutta.apievolution.jmh.json.provider.TestMethod100ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethod10ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethod250ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethod25ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethod500ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethod50ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethod75ProviderProxy;
import gutta.apievolution.jmh.json.provider.TestMethodEmptyProviderProxy;
import gutta.apievolution.json.provider.ProviderOperationProxy;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

/**
 * JMH benchmarks to determine the performance impact of format conversion in a JSON-based setting.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class JSONConversionBenchmarks extends JMHBenchmarkTemplate {

    private static final JsonRequestRouter ROUTER = createRouter();

    private static final ConsumerParameter CONSUMER_PARAMETER = new ConsumerParameter();

    private static JsonRequestRouter createRouter() {
        ProviderOperationProxy<?, ?>[] proxies = new ProviderOperationProxy<?, ?>[] {
                new TestMethodEmptyProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod10ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod25ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod50ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod75ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod100ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod250ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS),
                new TestMethod500ProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS) };

        return new JsonRequestRouter(DEFINITION_RESOLUTION, proxies);
    }

    private static final TestMethodEmptyConsumerProxy TEST_METHOD_EMPTY_PROXY = new TestMethodEmptyConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod10ConsumerProxy TEST_METHOD_10_PROXY = new TestMethod10ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod25ConsumerProxy TEST_METHOD_25_PROXY = new TestMethod25ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod50ConsumerProxy TEST_METHOD_50_PROXY = new TestMethod50ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod75ConsumerProxy TEST_METHOD_75_PROXY = new TestMethod75ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod100ConsumerProxy TEST_METHOD_100_PROXY = new TestMethod100ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod250ConsumerProxy TEST_METHOD_250_PROXY = new TestMethod250ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    private static final TestMethod500ConsumerProxy TEST_METHOD_500_PROXY = new TestMethod500ConsumerProxy(CONSUMER_API_DEFINITION, ROUTER);

    // Objects for JSON mapping

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private static final ProviderResult10 RESULT_10 = TestMethod10ProviderProxy.createResult();
    
    private static final ProviderResult25 RESULT_25 = TestMethod25ProviderProxy.createResult();
    
    private static final ProviderResult50 RESULT_50 = TestMethod50ProviderProxy.createResult();
    
    private static final ProviderResult75 RESULT_75 = TestMethod75ProviderProxy.createResult();
    
    private static final ProviderResult100 RESULT_100 = TestMethod100ProviderProxy.createResult();
    
    private static final ProviderResult250 RESULT_250 = TestMethod250ProviderProxy.createResult();
    
    private static final ProviderResult500 RESULT_500 = TestMethod500ProviderProxy.createResult();

    @Benchmark
    public void invokeEmptyResult_short() {
        TEST_METHOD_EMPTY_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }

    @Benchmark
    public void immediateJsonProcessing010_short() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_10);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult10.class);
    }

    @Benchmark
    public void indirectProcessing010_short() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_10);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult10.class);
    }

    @Benchmark
    public void invokeTestMethod010_short() {
        TEST_METHOD_10_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    public void immediateJsonProcessing025_short() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_25);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult25.class);
    }

    @Benchmark
    public void indirectProcessing025_short() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_25);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult25.class);
    }

    @Benchmark
    public void invokeTestMethod025_short() {
        TEST_METHOD_25_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    public void immediateJsonProcessing050_short() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_50);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult50.class);
    }

    @Benchmark
    public void indirectProcessing050_short() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_50);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult50.class);
    }

    @Benchmark
    public void invokeTestMethod050_short() {
        TEST_METHOD_50_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    public void immediateJsonProcessing075_short() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_75);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult75.class);
    }

    @Benchmark
    public void indirectProcessing075_short() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_75);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult75.class);
    }

    @Benchmark
    public void invokeTestMethod075_short() {
        TEST_METHOD_75_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    public void immediateJsonProcessing100_short() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_100);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult100.class);
    }

    @Benchmark
    public void indirectProcessing100_short() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_100);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult100.class);
    }

    @Benchmark
    public void invokeTestMethod100_short() {
        TEST_METHOD_100_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    public void immediateJsonProcessing250_long() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_250);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult250.class);
    }

    @Benchmark
    public void indirectProcessing250_long() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_250);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult250.class);
    }

    @Benchmark
    public void invokeTestMethod250_long() {
        TEST_METHOD_250_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    public void immediateJsonProcessing500_long() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_500);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult500.class);
    }

    @Benchmark
    public void indirectProcessing500_long() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_500);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult500.class);
    }

    @Benchmark
    public void invokeTestMethod500_long() {
        TEST_METHOD_500_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }

}
