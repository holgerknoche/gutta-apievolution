package gutta.apievolution.jmh.fixedformat;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.jmh.JMHBenchmarkTemplate;
import gutta.apievolution.jmh.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.jmh.fixedformat.consumer.EmptyTestMethodConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod100ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod10ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod250ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod25ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod500ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod50ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod75ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.provider.EmptyTestMethodProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult10;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult100;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult25;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult250;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult50;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult500;
import gutta.apievolution.jmh.fixedformat.provider.ProviderResult75;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod100ProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod10ProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod250ProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod25ProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod500ProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod50ProviderProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod75ProviderProxy;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * JMH benchmarks to determine the performance impact of format conversion with our fixed-format conversion.
 */
public class FixedFormatConversionBenchmarks extends JMHBenchmarkTemplate {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    private static final FixedFormatMapper MAPPER = new FixedFormatMapper();

    private static final ApiMappingScript CONSUMER_TO_PROVIDER_SCRIPT = new ApiMappingScriptGenerator().generateMappingScript(DEFINITION_RESOLUTION,
            MappingDirection.CONSUMER_TO_PROVIDER);
    
    private static final ApiMappingScript PROVIDER_TO_CONSUMER_SCRIPT = new ApiMappingScriptGenerator().generateMappingScript(DEFINITION_RESOLUTION,
            MappingDirection.PROVIDER_TO_CONSUMER);

    private static final RequestRouter REQUEST_ROUTER = createRequestRouter();
    
    private static RequestRouter createRequestRouter() {
        ProviderOperationProxy<?, ?>[] proxies = new ProviderOperationProxy<?, ?>[] {
            new EmptyTestMethodProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
        	new TestMethod10ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
        	new TestMethod25ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
        	new TestMethod50ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
        	new TestMethod75ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
            new TestMethod100ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
            new TestMethod250ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
            new TestMethod500ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET),
        };
        
        return new RequestRouter(proxies);
    }
    
    private static <T> ByteBuffer createData(Class<T> type, Supplier<T> valueSupplier) {
    	ByteBuffer buffer = ByteBuffer.allocate(MAPPER.determineMaxSizeOf(type));
    	FixedFormatData data = FixedFormatData.of(buffer, CHARSET);
    	MAPPER.writeValue(valueSupplier.get(), type, data);
    	
    	buffer.flip();
    	
    	return buffer;
    }
    
    private static final ConsumerParameter CONSUMER_PARAMETER = new ConsumerParameter();

    private static final EmptyTestMethodConsumerProxy TEST_METHOD_EMPTY_PROXY = new EmptyTestMethodConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);
    
    private static final TestMethod10ConsumerProxy TEST_METHOD_10_PROXY = new TestMethod10ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_10_DATA = createData(ProviderResult10.class, TestMethod10ProviderProxy::createResult);
    
    private static final TestMethod25ConsumerProxy TEST_METHOD_25_PROXY = new TestMethod25ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_25_DATA = createData(ProviderResult25.class, TestMethod25ProviderProxy::createResult);
    
    private static final TestMethod50ConsumerProxy TEST_METHOD_50_PROXY = new TestMethod50ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_50_DATA = createData(ProviderResult50.class, TestMethod50ProviderProxy::createResult);
    
    private static final TestMethod75ConsumerProxy TEST_METHOD_75_PROXY = new TestMethod75ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_75_DATA = createData(ProviderResult75.class, TestMethod75ProviderProxy::createResult);
    
    private static final TestMethod100ConsumerProxy TEST_METHOD_100_PROXY = new TestMethod100ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_100_DATA = createData(ProviderResult100.class, TestMethod100ProviderProxy::createResult);
    
    private static final TestMethod250ConsumerProxy TEST_METHOD_250_PROXY = new TestMethod250ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_250_DATA = createData(ProviderResult250.class, TestMethod250ProviderProxy::createResult);
    
    private static final TestMethod500ConsumerProxy TEST_METHOD_500_PROXY = new TestMethod500ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);    
    private static final ByteBuffer RESULT_500_DATA = createData(ProviderResult500.class, TestMethod500ProviderProxy::createResult);
    
    private static final ByteBuffer TARGET_BUFFER = ByteBuffer.allocate(32768);
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeEmptyResult() {
        TEST_METHOD_EMPTY_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly10() {
        ByteBuffer sourceBuffer = RESULT_10_DATA;
        sourceBuffer.position(0);
        
        ByteBuffer targetBuffer = TARGET_BUFFER;
        targetBuffer.clear();
        
        PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod10", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod10() {
        TEST_METHOD_10_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly25() {
        ByteBuffer sourceBuffer = RESULT_25_DATA;
        sourceBuffer.position(0);
        
        ByteBuffer targetBuffer = TARGET_BUFFER;
        targetBuffer.clear();
        
        PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod25", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod25() {
        TEST_METHOD_25_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly50() {
        ByteBuffer sourceBuffer = RESULT_50_DATA;
        sourceBuffer.position(0);
        
        ByteBuffer targetBuffer = TARGET_BUFFER;
        targetBuffer.clear();
        
        PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod50", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod50() {
        TEST_METHOD_50_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly75() {
        ByteBuffer sourceBuffer = RESULT_75_DATA;
        sourceBuffer.position(0);
        
        ByteBuffer targetBuffer = TARGET_BUFFER;
        targetBuffer.clear();
        
        PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod75", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod75() {
        TEST_METHOD_75_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly100() {
        ByteBuffer sourceBuffer = RESULT_100_DATA;
        sourceBuffer.position(0);
        
        ByteBuffer targetBuffer = TARGET_BUFFER;
        targetBuffer.clear();
        
        PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod100", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod100() {
        TEST_METHOD_100_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly250() {
    	ByteBuffer sourceBuffer = RESULT_250_DATA;
    	sourceBuffer.position(0);
    	
    	ByteBuffer targetBuffer = TARGET_BUFFER;
    	targetBuffer.clear();
    	
    	PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod250", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod250() {
        TEST_METHOD_250_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void resultConversionOnly500() {
        ByteBuffer sourceBuffer = RESULT_500_DATA;
        sourceBuffer.position(0);
        
        ByteBuffer targetBuffer = TARGET_BUFFER;
        targetBuffer.clear();
        
        PROVIDER_TO_CONSUMER_SCRIPT.mapResultFor("testMethod500", sourceBuffer, targetBuffer);
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod500() {
        TEST_METHOD_500_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
}
