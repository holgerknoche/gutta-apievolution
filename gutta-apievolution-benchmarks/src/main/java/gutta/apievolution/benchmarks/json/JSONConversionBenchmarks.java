package gutta.apievolution.benchmarks.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import gutta.apievolution.benchmarks.ExperimentSize;
import gutta.apievolution.benchmarks.JMHBenchmarkTemplate;
import gutta.apievolution.benchmarks.json.consumer.ConsumerParameter;
import gutta.apievolution.benchmarks.json.consumer.TestMethod100ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethod10ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethod250ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethod25ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethod500ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethod50ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethod75ConsumerProxy;
import gutta.apievolution.benchmarks.json.consumer.TestMethodEmptyConsumerProxy;
import gutta.apievolution.benchmarks.json.provider.ProviderResult10;
import gutta.apievolution.benchmarks.json.provider.ProviderResult100;
import gutta.apievolution.benchmarks.json.provider.ProviderResult25;
import gutta.apievolution.benchmarks.json.provider.ProviderResult250;
import gutta.apievolution.benchmarks.json.provider.ProviderResult50;
import gutta.apievolution.benchmarks.json.provider.ProviderResult500;
import gutta.apievolution.benchmarks.json.provider.ProviderResult75;
import gutta.apievolution.benchmarks.json.provider.TestMethod100ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethod10ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethod250ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethod25ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethod500ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethod50ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethod75ProviderProxy;
import gutta.apievolution.benchmarks.json.provider.TestMethodEmptyProviderProxy;
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
    @ExperimentSize(0)
    public void invokeEmptyResult() {
        TEST_METHOD_EMPTY_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }

    @Benchmark
    @ExperimentSize(10)
    public void immediateJsonProcessing010() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_10);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult10.class);
    }

    @Benchmark
    @ExperimentSize(10)
    public void indirectProcessing010() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_10);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult10.class);
    }

    @Benchmark
    @ExperimentSize(10)
    public void invokeTestMethod010() {
        TEST_METHOD_10_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @ExperimentSize(25)
    public void immediateJsonProcessing025() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_25);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult25.class);
    }

    @Benchmark
    @ExperimentSize(25)
    public void indirectProcessing025() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_25);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult25.class);
    }

    @Benchmark
    @ExperimentSize(25)
    public void invokeTestMethod025() {
        TEST_METHOD_25_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @ExperimentSize(50)
    public void immediateJsonProcessing050() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_50);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult50.class);
    }

    @Benchmark
    @ExperimentSize(50)
    public void indirectProcessing050() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_50);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult50.class);
    }

    @Benchmark
    @ExperimentSize(50)
    public void invokeTestMethod050() {
        TEST_METHOD_50_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @ExperimentSize(75)
    public void immediateJsonProcessing075() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_75);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult75.class);
    }

    @Benchmark
    @ExperimentSize(75)
    public void indirectProcessing075() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_75);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult75.class);
    }

    @Benchmark
    @ExperimentSize(75)
    public void invokeTestMethod075() {
        TEST_METHOD_75_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @ExperimentSize(100)
    public void immediateJsonProcessing100() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_100);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult100.class);
    }

    @Benchmark
    @ExperimentSize(100)
    public void indirectProcessing100() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_100);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult100.class);
    }

    @Benchmark
    @ExperimentSize(100)
    public void invokeTestMethod100() {
        TEST_METHOD_100_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @ExperimentSize(250)
    public void immediateJsonProcessing250() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_250);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult250.class);
    }

    @Benchmark
    @ExperimentSize(250)
    public void indirectProcessing250() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_250);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult250.class);
    }

    @Benchmark
    @ExperimentSize(250)
    public void invokeTestMethod250() {
        TEST_METHOD_250_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @ExperimentSize(500)
    public void immediateJsonProcessing500() throws IOException {
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(RESULT_500);
        OBJECT_MAPPER.readValue(jsonBytes, ProviderResult500.class);
    }

    @Benchmark
    @ExperimentSize(500)
    public void indirectProcessing500() throws IOException {
        JsonNode tree = OBJECT_MAPPER.valueToTree(RESULT_500);
        byte[] jsonBytes = OBJECT_MAPPER.writeValueAsBytes(tree);
        JsonNode readTree = OBJECT_MAPPER.readTree(jsonBytes);
        OBJECT_MAPPER.treeToValue(readTree, ProviderResult500.class);
    }

    @Benchmark
    @ExperimentSize(500)
    public void invokeTestMethod500() {
        TEST_METHOD_500_PROXY.invokeOperation(CONSUMER_PARAMETER);
    }

}
