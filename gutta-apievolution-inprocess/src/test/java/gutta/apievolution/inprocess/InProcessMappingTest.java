package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.inprocess.consumer.dynproxy.ConsumerApi;
import gutta.apievolution.inprocess.consumer.dynproxy.ConsumerEnum;
import gutta.apievolution.inprocess.consumer.dynproxy.ConsumerParameter;
import gutta.apievolution.inprocess.consumer.dynproxy.ConsumerResult;
import gutta.apievolution.inprocess.consumer.dynproxy.MappedConsumerException;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;
import gutta.apievolution.inprocess.dynproxy.MappedException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class InProcessMappingTest {
    
    static Stream<Arguments> mappingStrategies() {
        return Stream.of(
                Arguments.of(new DynamicProxyApiMappingStrategy())
                );
    }    
    
    @ParameterizedTest
    @MethodSource("mappingStrategies")
    void successfulInvocation(ApiMappingStrategy mappingStrategy) {
        ConsumerApi consumerApi = this.loadAndResolveApi(mappingStrategy);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestEnum(ConsumerEnum.VALUE_A);
        parameter.setTestField("someValue");
        parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        ConsumerResult result = consumerApi.testOperation(parameter);

        assertEquals(ConsumerEnum.VALUE_A, result.getResultEnum());
        assertEquals("someValue", result.getResultField());
        assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A), result.getResultList());
        assertEquals(42, result.getResultRecord().getField());
    }

    @ParameterizedTest
    @MethodSource("mappingStrategies")
    void invocationWithMappedException(ApiMappingStrategy mappingStrategy) {
        ConsumerApi consumerApi = this.loadAndResolveApi(mappingStrategy);
        
        MappedException exception = assertThrows(MappedException.class, () -> consumerApi.operationWithMappedException(new ConsumerParameter()));
        MappedConsumerException exceptionData = exception.getDataAs(MappedConsumerException.class).orElseThrow(NoSuchElementException::new);
        
        assertEquals("testException", exceptionData.getExceptionField());
    }

    private ConsumerApi loadAndResolveApi(ApiMappingStrategy mappingStrategy) {
     // Load the consumer and provider API definitions
        ConsumerApiDefinition consumerApiDefinition = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);

        RevisionHistory providerRevisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api",
                "apis/provider-revision-2.api");
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));

        // Create an API resolution context and an API resolver
        ApiResolutionContext resolutionContext = new ApiResolutionContext(consumerApiDefinition, providerRevisionHistory,
                supportedRevisions,
                new DefaultTypeToClassMap("gutta.apievolution.inprocess.consumer.dynproxy", "gutta.apievolution.inprocess.provider"));
        ApiResolver apiResolver = new ApiResolver(resolutionContext, mappingStrategy);

        // Resolve the API
        return apiResolver.resolveApi(ConsumerApi.class);
    }

}
