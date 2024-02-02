package gutta.apievolution.inprocess.customerexample.consumer.objectmapping.v6;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import gutta.apievolution.inprocess.customerexample.CustomerExampleTestTemplate;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;

class CustomerExampleV6Test extends CustomerExampleTestTemplate {

	@Test
    void invokeProviderV6FromConsumerV6() {        
        StreetAddress primaryAddress = new StreetAddress();
        primaryAddress.setStreet("Test Street");
        primaryAddress.setNumber(1234);
        primaryAddress.setPostalCode(5678);
        primaryAddress.setCity("Test City");
        
        StreetAddress secondaryAddress1 = new StreetAddress();
        secondaryAddress1.setStreet("Test Road");
        secondaryAddress1.setNumber(235);
        secondaryAddress1.setPostalCode(7654);
        secondaryAddress1.setCity("Test City");
        
        POBoxAddress secondaryAddress2 = new POBoxAddress();
        secondaryAddress2.setBoxNo(246);
        secondaryAddress2.setPostalCode(9876);
        secondaryAddress2.setCity("Test City");
        
        Customer customer = new Customer();
        customer.setFirstName("Test");
        customer.setLastName("Tester");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(Gender.THIRD);
        customer.setPrimaryAddress(primaryAddress);
        customer.setSecondaryAddresses(Arrays.asList(secondaryAddress1, secondaryAddress2));
        
        ConsumerApi customerApi = this.createApi(ConsumerApi.class, this.getClass().getPackage(), CONSUMER_API_V6,
				new ObjectMappingApiMappingStrategy());

		Customer result = customerApi.upsert(customer);

		assertNotSame(customer, result);
		assertEquals(customer, result);
	}
	
}
