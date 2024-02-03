package gutta.apievolution.customerexample.inprocess.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(AddressImpl.class)
public interface Address {
    
    Integer getPostalCode();
    
    void setPostalCode(Integer postalCode);
    
    String getCity();
    
    void setCity(String city);

}
