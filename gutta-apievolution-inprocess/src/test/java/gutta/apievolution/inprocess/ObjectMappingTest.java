package gutta.apievolution.inprocess;

import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerApi;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerEnum;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerParameter;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerResult;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerSubType;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerSuperType;
import gutta.apievolution.inprocess.consumer.objectmapping.MappedConsumerException;
import gutta.apievolution.inprocess.dynproxy.MappedException;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;
import gutta.apievolution.inprocess.provider.UnmappedTestException;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for the type mapping strategy that creates copies of objects.
 */
class ObjectMappingTest extends InProcessMappingTestTemplate<ObjectMappingApiMappingStrategy> {

    /**
     * Test case: Successful invocation of an API method.
     */
    @Test
    void successfulInvocation() {
        ConsumerApi consumerApi = this.loadAndResolveApi();

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
        ConsumerApi consumerApi = this.loadAndResolveApi();

        MappedConsumerException exception = assertThrows(MappedConsumerException.class,
                () -> consumerApi.operationWithMappedException(new ConsumerParameter()));
        assertEquals("testException", exception.getExceptionField());
    }

    /**
     * Test case: An invocation that throws an unmapped, but modeled exception works as expected, namely wrapping the exception in an {@link UnmappedException}.
     */
    @Test
    void invocationWithUnmappedException() {
        ConsumerApi consumerApi = this.loadAndResolveApi();

        UnmappedException exception = assertThrows(UnmappedException.class, () -> consumerApi.operationWithUnmappedException(new ConsumerParameter()));
        assertEquals(UnmappedTestException.class, exception.getCause().getClass());
    }

    /**
     * Test case: An invocation that throws an unmapped and unmodeled exception works as expected, namely wrapping the exception in an
     * {@link UnmappedException}.
     */
    @Test
    void invocationWithUnmodeledException() {
        ConsumerApi consumerApi = this.loadAndResolveApi();

        UnmappedException exception = assertThrows(UnmappedException.class, () -> consumerApi.operationWithRuntimeException(new ConsumerParameter()));
        assertEquals(UnsupportedOperationException.class, exception.getCause().getClass());
    }
    
    /**
     * Test case: Invocation of a method that returns a representable subtype of the result type.
     */
    @Test
    void invocationWithRepresentableSubtype() {
        ConsumerApi consumerApi = this.loadAndResolveApi();
        
        ConsumerSubType result = (ConsumerSubType) consumerApi.operationWithRepresentableSubtype(new ConsumerParameter());
        
        assertTrue(result.isRepresentable());
        assertEquals(1234, result.getInheritedField());
        assertEquals(5678, result.getSubField());
    }
    
    /**
     * Test case: Invocation of a method that returns an unrepresentable subtype of the result type.
     */
    @Test
    void invocationWithUnrepresentableSubtype() {
        ConsumerApi consumerApi = this.loadAndResolveApi();
        
        ConsumerSuperType result = consumerApi.operationWithUnrepresentableSubtype(new ConsumerParameter());
        assertFalse(result.isRepresentable());
    }
    
    /**
     * Test case: Invocation of a method that consumes and returns a polymorphic type.
     */
    @Test
    void invocationWithPolymorphicTypes() {
        ConsumerSubType parameter = new ConsumerSubType();
        parameter.setInheritedField(1234);
        parameter.setSubField(5678);

        ConsumerApi consumerApi = this.loadAndResolveApi();
        ConsumerSuperType result = consumerApi.polyOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter.getInheritedField(), result.getInheritedField());
    }

    private ConsumerApi loadAndResolveApi() {
        return this.loadAndResolveApi(ConsumerApi.class, "gutta.apievolution.inprocess.consumer.objectmapping");
    }

    @Override
    protected ObjectMappingApiMappingStrategy apiMappingStrategy() {
        return new ObjectMappingApiMappingStrategy();
    }

}
