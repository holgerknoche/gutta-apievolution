package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(AddressImpl.class)
public interface Address {
    
    Integer getPostalCode();
    
    void setPostalCode(Integer postalCode);
    
    String getCity();
    
    void setCity(String city);

}
