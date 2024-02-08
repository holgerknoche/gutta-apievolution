package gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v1;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import org.junit.jupiter.api.Test;

import gutta.apievolution.customerexample.inprocess.consumer.CustomerExampleInprocessTestTemplate;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;

class CustomerExampleV1Test extends CustomerExampleInprocessTestTemplate {

	@Test
	void invokeProviderV6FromConsumerV1() {
		Address address = new AddressImpl();
		address.setStreet("Test Street");
		address.setNumber(1234);
		address.setPostalCode(5678);
		address.setCity("Test City");

		Customer customer = new CustomerImpl();
		customer.setFirstName("Test");
		customer.setLastName("Tester");
		customer.setGender(1);
		customer.setAddress(address);

		ConsumerApi customerApi = this.createApi(ConsumerApi.class, this.getClass().getPackage(), CONSUMER_API_V1,
				new DynamicProxyApiMappingStrategy());

		Customer result = customerApi.upsert(customer);

		assertNotSame(customer, result);
		
		// We cannot use equals on the objects directly due to the dynamic proxies
		assertEquals(customer.getFirstName(), result.getFirstName());
		assertEquals(customer.getLastName(), result.getLastName());
		assertEquals(customer.getGender(), result.getGender());
		
		Address resultAddress = result.getAddress();
		assertEquals(address.getCity(), resultAddress.getCity());
		assertEquals(address.getNumber(), resultAddress.getNumber());
		assertEquals(address.getPostalCode(), resultAddress.getPostalCode());
		assertEquals(address.getStreet(), resultAddress.getStreet());
	}

}
