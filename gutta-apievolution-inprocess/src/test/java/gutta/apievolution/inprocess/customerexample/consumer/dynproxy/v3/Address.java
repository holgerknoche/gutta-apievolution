package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v3;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(AddressImpl.class)
public interface Address {
    
    String getStreet();
    
    void setStreet(String street);
    
    Integer getNumber();
    
    void setNumber(Integer number);
    
    Integer getPostalCode();
    
    void setPostalCode(Integer postalCode);
    
    String getCity();
    
    void setCity(String city);

}
