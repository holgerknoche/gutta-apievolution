package gutta.apievolution.jmh.inprocess.dynproxy;

import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;
import gutta.apievolution.jmh.inprocess.InProcessConversionBenchmarkTemplate;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

public class DynamicProxyConversionBenchmarks extends InProcessConversionBenchmarkTemplate {

    private static final ConsumerApi CONSUMER_API = createConsumerApi(ConsumerApi.class, new DynamicProxyApiMappingStrategy(),
            "gutta.apievolution.jmh.inprocess.dynproxy");
    
    private static final ConsumerParameter CONSUMER_PARAMETER = new ConsumerParameterImpl();
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeResult10() {
        CONSUMER_API.testMethod10(CONSUMER_PARAMETER);                    
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeAndInspectResult10() {
        ConsumerResult10 result = CONSUMER_API.testMethod10(CONSUMER_PARAMETER);
        
        result.getIntField1();
        result.getIntField2();
        result.getIntField3();
        result.getIntField4();
        result.getIntField5();
        result.getIntField6();
        result.getIntField7();
        result.getIntField8();
        result.getIntField9();
        result.getIntField10();
        result.getStringField1();
        result.getStringField2();
        result.getStringField3();
        result.getStringField4();
        result.getStringField5();
        result.getStringField6();
        result.getStringField7();
        result.getStringField8();
        result.getStringField9();
        result.getStringField10();
    }
        
}
