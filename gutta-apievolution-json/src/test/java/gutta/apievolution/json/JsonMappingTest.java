package gutta.apievolution.json;

import gutta.apievolution.json.consumer.ConsumerEnum;
import gutta.apievolution.json.consumer.ConsumerParameter;
import gutta.apievolution.json.consumer.ConsumerResult;
import gutta.apievolution.json.consumer.ConsumerStructureWithPolyField;
import gutta.apievolution.json.consumer.ConsumerSubTypeA;
import gutta.apievolution.json.consumer.ConsumerSubTypeB;
import gutta.apievolution.json.consumer.ConsumerSuperType;
import gutta.apievolution.json.consumer.PolyOperation2ConsumerProxy;
import gutta.apievolution.json.consumer.PolyOperationConsumerProxy;
import gutta.apievolution.json.consumer.TestOperationConsumerProxy;
import gutta.apievolution.json.provider.PolyOperation2ProviderProxy;
import gutta.apievolution.json.provider.PolyOperationProviderProxy;
import gutta.apievolution.json.provider.TestOperationProviderProxy;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

/**
 * Test case for the JSON mapping functionality.
 */
class JsonMappingTest {

    /**
     * Tests a simple, JSON-based conversation between a consumer and a provider.
     */
    @Test
    void testJsonConversation() {
        TestRequestRouter requestRouter = new TestRequestRouter("test.provider");

        TestOperationProviderProxy serviceProxy = new TestOperationProviderProxy();
        requestRouter.registerProviderService(serviceProxy);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestField("test value");
        parameter.setTestEnum(ConsumerEnum.VALUE_A);
        parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        TestOperationConsumerProxy providerProxy = new TestOperationConsumerProxy(requestRouter);
        ConsumerResult result = providerProxy.invokeOperation(parameter);

        assertEquals("test valueX", result.getResultField());
        assertEquals(ConsumerEnum.VALUE_B, result.getResultEnum());
        assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A), result.getResultList());
    }
    
    /**
     * Test case: The invocation of a method with polymorphic parameter and result works as expected.
     */
    @Test
    void immediatePolymorphicTypes() {
        PolyOperationProviderProxy providerProxy = new PolyOperationProviderProxy();
        RequestRouter router = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerSubTypeB parameter = new ConsumerSubTypeB();
        parameter.setFieldB(1234);
        
        PolyOperationConsumerProxy consumerProxy = new PolyOperationConsumerProxy(router);
        ConsumerSuperType result = consumerProxy.invokeOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter, result);
    }

    /**
     * Test case: The invocation of a method with parameter and result structures that contain polymorphic values works as expected.
     */
    @Test
    void containedPolymorphicTypes() {
        PolyOperation2ProviderProxy providerProxy = new PolyOperation2ProviderProxy();
        RequestRouter router = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerSubTypeA element = new ConsumerSubTypeA();
        element.setFieldA("Test");
        
        ConsumerStructureWithPolyField parameter = new ConsumerStructureWithPolyField();
        parameter.setField(element);
        
        PolyOperation2ConsumerProxy consumerProxy = new PolyOperation2ConsumerProxy(router);
        ConsumerStructureWithPolyField result = consumerProxy.invokeOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter, result);
    }

}
