package gutta.apievolution.inprocess;

import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerApi;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerEnum;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerParameter;
import gutta.apievolution.inprocess.consumer.objectmapping.ConsumerResult;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

/**
 * Test cases for the type mapping strategy that creates copies of objects.
 */
class ObjectMappingTest extends InProcessMappingTestTemplate<ObjectMappingApiMappingStrategy> {

    @Test
    void successfulInvocation() {
        ConsumerApi consumerApi = this.loadAndResolveApi(ConsumerApi.class);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestEnum(ConsumerEnum.VALUE_A);
        parameter.setTestField("someValue");
        parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        ConsumerResult result = consumerApi.testOperation(parameter);
        
        System.out.println(result);
//
//        assertEquals(ConsumerEnum.VALUE_A, result.getResultEnum());
//        assertEquals("someValue", result.getResultField());
//        assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A), result.getResultList());
//        assertEquals(42, result.getResultRecord().getField());
    }

    @Override
    protected ObjectMappingApiMappingStrategy apiMappingStrategy() {
        return new ObjectMappingApiMappingStrategy();
    }

}
