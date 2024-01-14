package gutta.apievolution.fixedformat.customerexample.consumer.v1;

import gutta.apievolution.fixedformat.customerexample.CustomerExampleTestTemplate;
import org.junit.jupiter.api.Test;

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
        System.out.println(result);
    }
    
}
