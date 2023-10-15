package gutta.apievolution.inprocess;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.inprocess.consumer.ConsumerApi;
import gutta.apievolution.inprocess.consumer.ConsumerEnum;
import gutta.apievolution.inprocess.consumer.ConsumerParameter;
import gutta.apievolution.inprocess.consumer.ConsumerResult;

class InProcessMappingTest {

	@Test
	void successfulInvocation() {
		ConsumerApiDefinition consumerApiDefinition = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 1);		
		ConsumerApi consumerApi = ApiResolver.resolveApi(consumerApiDefinition, ConsumerApi.class);
		
		ConsumerParameter parameter = new ConsumerParameter();
		parameter.setTestEnum(ConsumerEnum.VALUE_A);
		parameter.setTestField("someValue");
		parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));
	
		ConsumerResult result = consumerApi.testOperation(parameter);
		
		//assertEquals(ConsumerEnum.VALUE_A, result.getResultEnum());
		//assertEquals("someValue", result.getRetField());
		//assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A), result.getResultList());
	}
		
}
