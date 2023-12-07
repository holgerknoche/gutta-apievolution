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
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeResult100() {
        CONSUMER_API.testMethod100(CONSUMER_PARAMETER);                    
    }
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeAndInspectResult100() {
        ConsumerResult100 result = CONSUMER_API.testMethod100(CONSUMER_PARAMETER);
        
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
        result.getIntField11();
        result.getIntField12();
        result.getIntField13();
        result.getIntField14();
        result.getIntField15();
        result.getIntField16();
        result.getIntField17();
        result.getIntField18();
        result.getIntField19();
        result.getIntField20();
        result.getIntField21();
        result.getIntField22();
        result.getIntField23();
        result.getIntField24();
        result.getIntField25();
        result.getIntField26();
        result.getIntField27();
        result.getIntField28();
        result.getIntField29();
        result.getIntField30();
        result.getIntField31();
        result.getIntField32();
        result.getIntField33();
        result.getIntField34();
        result.getIntField35();
        result.getIntField36();
        result.getIntField37();
        result.getIntField38();
        result.getIntField39();
        result.getIntField40();
        result.getIntField41();
        result.getIntField42();
        result.getIntField43();
        result.getIntField44();
        result.getIntField45();
        result.getIntField46();
        result.getIntField47();
        result.getIntField48();
        result.getIntField49();
        result.getIntField50();
        result.getIntField51();
        result.getIntField52();
        result.getIntField53();
        result.getIntField54();
        result.getIntField55();
        result.getIntField56();
        result.getIntField57();
        result.getIntField58();
        result.getIntField59();
        result.getIntField60();
        result.getIntField61();
        result.getIntField62();
        result.getIntField63();
        result.getIntField64();
        result.getIntField65();
        result.getIntField66();
        result.getIntField67();
        result.getIntField68();
        result.getIntField69();
        result.getIntField70();
        result.getIntField71();
        result.getIntField72();
        result.getIntField73();
        result.getIntField74();
        result.getIntField75();
        result.getIntField76();
        result.getIntField77();
        result.getIntField78();
        result.getIntField79();
        result.getIntField80();
        result.getIntField81();
        result.getIntField82();
        result.getIntField83();
        result.getIntField84();
        result.getIntField85();
        result.getIntField86();
        result.getIntField87();
        result.getIntField88();
        result.getIntField89();
        result.getIntField90();
        result.getIntField91();
        result.getIntField92();
        result.getIntField93();
        result.getIntField94();
        result.getIntField95();
        result.getIntField96();
        result.getIntField97();
        result.getIntField98();
        result.getIntField99();
        result.getIntField100();
        
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
        result.getStringField11();
        result.getStringField12();
        result.getStringField13();
        result.getStringField14();
        result.getStringField15();
        result.getStringField16();
        result.getStringField17();
        result.getStringField18();
        result.getStringField19();
        result.getStringField20();
        result.getStringField21();
        result.getStringField22();
        result.getStringField23();
        result.getStringField24();
        result.getStringField25();
        result.getStringField26();
        result.getStringField27();
        result.getStringField28();
        result.getStringField29();
        result.getStringField30();
        result.getStringField31();
        result.getStringField32();
        result.getStringField33();
        result.getStringField34();
        result.getStringField35();
        result.getStringField36();
        result.getStringField37();
        result.getStringField38();
        result.getStringField39();
        result.getStringField40();
        result.getStringField41();
        result.getStringField42();
        result.getStringField43();
        result.getStringField44();
        result.getStringField45();
        result.getStringField46();
        result.getStringField47();
        result.getStringField48();
        result.getStringField49();
        result.getStringField50();
        result.getStringField51();
        result.getStringField52();
        result.getStringField53();
        result.getStringField54();
        result.getStringField55();
        result.getStringField56();
        result.getStringField57();
        result.getStringField58();
        result.getStringField59();
        result.getStringField60();
        result.getStringField61();
        result.getStringField62();
        result.getStringField63();
        result.getStringField64();
        result.getStringField65();
        result.getStringField66();
        result.getStringField67();
        result.getStringField68();
        result.getStringField69();
        result.getStringField70();
        result.getStringField71();
        result.getStringField72();
        result.getStringField73();
        result.getStringField74();
        result.getStringField75();
        result.getStringField76();
        result.getStringField77();
        result.getStringField78();
        result.getStringField79();
        result.getStringField80();
        result.getStringField81();
        result.getStringField82();
        result.getStringField83();
        result.getStringField84();
        result.getStringField85();
        result.getStringField86();
        result.getStringField87();
        result.getStringField88();
        result.getStringField89();
        result.getStringField90();
        result.getStringField91();
        result.getStringField92();
        result.getStringField93();
        result.getStringField94();
        result.getStringField95();
        result.getStringField96();
        result.getStringField97();
        result.getStringField98();
        result.getStringField99();
        result.getStringField100();
    }
        
}
