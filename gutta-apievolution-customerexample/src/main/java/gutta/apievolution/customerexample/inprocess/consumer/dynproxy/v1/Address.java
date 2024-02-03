package gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v1;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(Address.class)
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
