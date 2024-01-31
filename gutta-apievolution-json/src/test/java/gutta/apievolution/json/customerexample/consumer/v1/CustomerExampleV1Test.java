package gutta.apievolution.json.customerexample.consumer.v1;

import gutta.apievolution.json.customerexample.CustomerExampleTestTemplate;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class CustomerExampleV1Test extends CustomerExampleTestTemplate {
    
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
        
        Customer result = this.invokeProviderMethod(CONSUMER_API_V1, CONSUMER_API_ID_V1, UpsertOperationConsumerProxyV1::new, customer);                
        
        assertNotSame(customer, result);        
        assertEquals(customer, result);
    }
    
}
