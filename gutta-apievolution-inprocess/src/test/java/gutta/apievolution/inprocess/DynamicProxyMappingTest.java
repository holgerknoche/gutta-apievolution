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
import gutta.apievolution.inprocess.dynproxy.UnmappedException;
import gutta.apievolution.inprocess.provider.UnmappedTestException;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for the type mapping strategy using dynamic proxies.
 */
class DynamicProxyMappingTest {    
    
    /**
     * Test case: A successful invocation (i.e., one not throwing an exception) works as expected.
     */
    @Test
    void successfulInvocation() {
        ConsumerApi consumerApi = this.loadAndResolveApi(new DynamicProxyApiMappingStrategy());

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

    /**
     * Test case: An invocation that throws a mapped exception works as expected, namely wrapping the mapped exception data in a {@link MappedException}.
     */
    @Test
    void invocationWithMappedException() {
        ConsumerApi consumerApi = this.loadAndResolveApi(new DynamicProxyApiMappingStrategy());
        
        MappedException exception = assertThrows(MappedException.class, () -> consumerApi.operationWithMappedException(new ConsumerParameter()));
        MappedConsumerException exceptionData = exception.getDataAs(MappedConsumerException.class).orElseThrow(NoSuchElementException::new);
        
        assertEquals("testException", exceptionData.getExceptionField());
    }
    
    /**
     * Test case: An invocation that throws an unmapped, but modeled exception works as expected, namely wrapping the exception in an {@link UnmappedException}. 
     */
    @Test
    void invocationWithUnmappedException() {
        ConsumerApi consumerApi = this.loadAndResolveApi(new DynamicProxyApiMappingStrategy());
        
        UnmappedException exception = assertThrows(UnmappedException.class, () -> consumerApi.operationWithUnmappedException(new ConsumerParameter()));
        assertEquals(UnmappedTestException.class, exception.getCause().getClass());
    }
    
    /**
     * Test case: An invocation that throws an unmapped and unmodeled exception works as expected, namely wrapping the exception in an {@link UnmappedException}.
     */
    @Test
    void invocationWithUnmodeledException() {
        ConsumerApi consumerApi = this.loadAndResolveApi(new DynamicProxyApiMappingStrategy());
        
        UnmappedException exception = assertThrows(UnmappedException.class, () -> consumerApi.operationWithRuntimeException(new ConsumerParameter()));
        assertEquals(UnsupportedOperationException.class, exception.getCause().getClass());
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
