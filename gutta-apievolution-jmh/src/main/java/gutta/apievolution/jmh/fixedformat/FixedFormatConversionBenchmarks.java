package gutta.apievolution.jmh.fixedformat;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.jmh.JMHBenchmarkTemplate;
import gutta.apievolution.jmh.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.jmh.fixedformat.consumer.TestMethod100ConsumerProxy;
import gutta.apievolution.jmh.fixedformat.provider.TestMethod100ProviderProxy;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;

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
            new TestMethod100ProviderProxy(PROVIDER_TO_CONSUMER_SCRIPT, CONSUMER_TO_PROVIDER_SCRIPT, MAPPER, CHARSET)
        };
        
        return new RequestRouter(proxies);
    }
    
    private static final TestMethod100ConsumerProxy TEST_METHOD_100_PROXY = new TestMethod100ConsumerProxy(REQUEST_ROUTER, MAPPER, CHARSET);
    
    private static final ConsumerParameter CONSUMER_PARAMETER = new ConsumerParameter();
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeTestMethod100() {
        TEST_METHOD_100_PROXY.invoke(CONSUMER_PARAMETER);
    }
    
}
