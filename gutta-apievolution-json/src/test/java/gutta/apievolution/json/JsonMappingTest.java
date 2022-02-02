package gutta.apievolution.json;

import gutta.apievolution.json.consumer.ConsumerEnum;
import gutta.apievolution.json.consumer.ConsumerParameter;
import gutta.apievolution.json.consumer.ConsumerResult;
import gutta.apievolution.json.consumer.TestProviderProxy;
import gutta.apievolution.json.provider.TestProviderServiceProxy;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for the JSON mapping functionality.
 */
class JsonMappingTest {

    /**
     * Tests a simple, JSON-based conversation between a consumer and a provider.
     */
    @Test
    void testJsonConversation() {
        TestRequestRouter requestRouter = new TestRequestRouter();

        TestProviderServiceProxy serviceProxy = new TestProviderServiceProxy();
        requestRouter.registerProviderService(serviceProxy);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestField("test value");
        parameter.setTestEnum(ConsumerEnum.VALUE_A);
        parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        TestProviderProxy providerProxy = new TestProviderProxy(requestRouter);
        ConsumerResult result = providerProxy.invokeProviderMethod(parameter);

        assertEquals("test valueX", result.getResultField());
        assertEquals(ConsumerEnum.VALUE_B, result.getResultEnum());
        assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A), result.getResultList());
    }

}
