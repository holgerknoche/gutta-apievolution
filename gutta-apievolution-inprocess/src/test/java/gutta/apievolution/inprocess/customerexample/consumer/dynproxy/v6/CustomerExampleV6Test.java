package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

import gutta.apievolution.inprocess.customerexample.CustomerExampleTestTemplate;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.fail;

class CustomerExampleV6Test extends CustomerExampleTestTemplate {

	@Test
	void invokeProviderV6FromConsumerV6() {
	    StreetAddress primaryAddress = new StreetAddressImpl();
        primaryAddress.setStreet("Test Street");
        primaryAddress.setNumber(1234);
        primaryAddress.setPostalCode(5678);
        primaryAddress.setCity("Test City");
        
        StreetAddress secondaryAddress1 = new StreetAddressImpl();
        secondaryAddress1.setStreet("Test Road");
        secondaryAddress1.setNumber(235);
        secondaryAddress1.setPostalCode(7654);
        secondaryAddress1.setCity("Test City");
        
        POBoxAddress secondaryAddress2 = new POBoxAddressImpl();
        secondaryAddress2.setBoxNo(246);
        secondaryAddress2.setPostalCode(9876);
        secondaryAddress2.setCity("Test City");
        
        Customer customer = new CustomerImpl();
        customer.setFirstName("Test");
        customer.setLastName("Tester");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(Gender.THIRD);
        customer.setPrimaryAddress(primaryAddress);
        customer.setSecondaryAddresses(Arrays.asList(secondaryAddress1, secondaryAddress2));
        
        ConsumerApi customerApi = this.createApi(ConsumerApi.class, this.getClass().getPackage(), CONSUMER_API_V6,
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
        assertEquals(expected.getPostalCode(), actual.getPostalCode());
        
        if (expected instanceof StreetAddress && actual instanceof StreetAddress) {
            assertEqualStreetAddresses((StreetAddress) expected, (StreetAddress) actual);
        } else if (expected instanceof POBoxAddress && actual instanceof POBoxAddress) {
            assertEqualPOBoxAddresses((POBoxAddress) expected, (POBoxAddress) actual);
        } else {
            fail();
        }
	}
	
	private static void assertEqualStreetAddresses(StreetAddress expected, StreetAddress actual) {
	    assertEquals(expected.getNumber(), actual.getNumber());
	    assertEquals(expected.getStreet(), actual.getStreet());
	}
	
	private static void assertEqualPOBoxAddresses(POBoxAddress expected, POBoxAddress actual) {
	    assertEquals(expected.getBoxNo(), actual.getBoxNo());
	}

}
