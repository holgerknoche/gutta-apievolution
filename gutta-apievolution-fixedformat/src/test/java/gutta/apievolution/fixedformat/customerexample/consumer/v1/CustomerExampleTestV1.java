package gutta.apievolution.fixedformat.customerexample.consumer.v1;

import gutta.apievolution.fixedformat.customerexample.CustomerExampleTestTemplate;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class CustomerExampleTestV1 extends CustomerExampleTestTemplate {
    
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
        
        assertEquals(customer.getFirstName(), result.getFirstName());
        assertEquals(customer.getLastName(), result.getLastName());
        assertEquals(customer.getGender(), result.getGender());
       
        Address resultAddress = customer.getAddress();
        assertEquals(address.getStreet(), resultAddress.getStreet());
        assertEquals(address.getNumber(), resultAddress.getNumber());
        assertEquals(address.getPostalCode(), resultAddress.getPostalCode());
        assertEquals(address.getCity(), resultAddress.getCity());
    }
    
}
