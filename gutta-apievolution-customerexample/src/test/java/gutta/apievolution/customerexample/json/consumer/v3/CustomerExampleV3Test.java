package gutta.apievolution.customerexample.json.consumer.v3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import gutta.apievolution.customerexample.json.consumer.CustomerExampleJsonTestTemplate;

class CustomerExampleV3Test extends CustomerExampleJsonTestTemplate {

	@Test
    void invokeProviderV6FromConsumerV3() {        
        Address primaryAddress = new Address();
        primaryAddress.setStreet("Test Street");
        primaryAddress.setNumber(1234);
        primaryAddress.setPostalCode(5678);
        primaryAddress.setCity("Test City");
        
        Address secondaryAddress1 = new Address();
        secondaryAddress1.setStreet("Test Road");
        secondaryAddress1.setNumber(235);
        secondaryAddress1.setPostalCode(7654);
        secondaryAddress1.setCity("Test City");
        
        Address secondaryAddress2 = new Address();
        secondaryAddress2.setStreet("Test Road");
        secondaryAddress2.setNumber(123);
        secondaryAddress2.setPostalCode(9876);
        secondaryAddress2.setCity("Test City");
        
        Customer customer = new Customer();
        customer.setFirstName("Test");
        customer.setLastName("Tester");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(1);
        customer.setPrimaryAddress(primaryAddress);
        customer.setSecondaryAddresses(Arrays.asList(secondaryAddress1, secondaryAddress2));
        
        Customer result = this.invokeProviderMethod(CONSUMER_API_V3, CONSUMER_API_ID_V3, UpsertOperationConsumerProxyV3::new, customer);                
        
        assertNotSame(customer, result);
        assertEquals(customer, result);
    }
		
}
