package gutta.apievolution.customerexample.fixedformat.consumer.v1;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import org.junit.jupiter.api.Test;

import gutta.apievolution.customerexample.fixedformat.consumer.CustomerExampleFixedFormatTestTemplate;

class CustomerExampleV1Test extends CustomerExampleFixedFormatTestTemplate {
    
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
        
        Customer result = this.invokeProviderMethod(CONSUMER_API_V1, UpsertOperationConsumerProxyV1::new, customer);                
        
        assertNotSame(customer, result);        
        assertEquals(customer, result);
    }
    
}
