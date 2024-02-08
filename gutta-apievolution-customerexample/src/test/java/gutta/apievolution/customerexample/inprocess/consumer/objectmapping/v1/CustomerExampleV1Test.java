package gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v1;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import org.junit.jupiter.api.Test;

import gutta.apievolution.customerexample.inprocess.consumer.CustomerExampleInprocessTestTemplate;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;

class CustomerExampleV1Test extends CustomerExampleInprocessTestTemplate {

	@Test
	void invokeProviderV6FromConsumerV1() {
		Address address = new Address();
		address.setStreet("Test Street");
		address.setNumber(1234);
		address.setPostalCode(5678);
		address.setCity("Test City");

		Customer customer = new Customer();
		customer.setFirstName("Test");
		customer.setLastName("Tester");
		customer.setGender(1);
		customer.setAddress(address);

		ConsumerApi customerApi = this.createApi(ConsumerApi.class, this.getClass().getPackage(), CONSUMER_API_V1,
				new ObjectMappingApiMappingStrategy());

		Customer result = customerApi.upsert(customer);

		assertNotSame(customer, result);
		assertEquals(customer, result);
	}

}
