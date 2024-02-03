package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v3;

import gutta.apievolution.inprocess.customerexample.CustomerExampleTestTemplate;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class CustomerExampleV3Test extends CustomerExampleTestTemplate {

	@Test
	void invokeProviderV6FromConsumerV3() {
	    Address primaryAddress = new AddressImpl();
        primaryAddress.setStreet("Test Street");
        primaryAddress.setNumber(1234);
        primaryAddress.setPostalCode(5678);
        primaryAddress.setCity("Test City");
        
        Address secondaryAddress1 = new AddressImpl();
        secondaryAddress1.setStreet("Test Road");
        secondaryAddress1.setNumber(235);
        secondaryAddress1.setPostalCode(7654);
        secondaryAddress1.setCity("Test City");
        
        Address secondaryAddress2 = new AddressImpl();
        secondaryAddress2.setStreet("Test Road");
        secondaryAddress2.setNumber(123);
        secondaryAddress2.setPostalCode(9876);
        secondaryAddress2.setCity("Test City");
        
        Customer customer = new CustomerImpl();
        customer.setFirstName("Test");
        customer.setLastName("Tester");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(1);
        customer.setPrimaryAddress(primaryAddress);
        customer.setSecondaryAddresses(Arrays.asList(secondaryAddress1, secondaryAddress2));

        ConsumerApi customerApi = this.createApi(ConsumerApi.class, this.getClass().getPackage(), CONSUMER_API_V3,
                new DynamicProxyApiMappingStrategy());

        Customer result = customerApi.upsert(customer);

        assertNotSame(customer, result);
		
		// We cannot use equals on the objects directly due to the dynamic proxies
		assertEquals(customer.getFirstName(), result.getFirstName());
		assertEquals(customer.getLastName(), result.getLastName());
		assertEquals(customer.getDateOfBirth(), result.getDateOfBirth());
		assertEquals(customer.getGender(), result.getGender());
		
		Address resultPrimaryAddress = result.getPrimaryAddress();
		assertEqualAddresses(primaryAddress, resultPrimaryAddress);
		
		assertEquals(2, result.getSecondaryAddresses().size());
		Address resultSecondaryAddress1 = result.getSecondaryAddresses().get(0);
		Address resultSecondaryAddress2 = result.getSecondaryAddresses().get(1);
		
		assertEqualAddresses(secondaryAddress1, resultSecondaryAddress1);
		assertEqualAddresses(secondaryAddress2, resultSecondaryAddress2);
	}
	
	private static void assertEqualAddresses(Address expected, Address actual) {
	    assertEquals(expected.getCity(), actual.getCity());
        assertEquals(expected.getNumber(), actual.getNumber());
        assertEquals(expected.getPostalCode(), actual.getPostalCode());
        assertEquals(expected.getStreet(), actual.getStreet());
	}

}
