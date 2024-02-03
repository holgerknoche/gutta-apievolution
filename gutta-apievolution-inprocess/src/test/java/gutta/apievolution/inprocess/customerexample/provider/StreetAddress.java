package gutta.apievolution.inprocess.customerexample.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(StreetAddressImpl.class)
public interface StreetAddress extends Address {

    String getStreet();
    
    void setStreet(String street);
    
    Integer getNumber();
    
    void setNumber(Integer number);
    
}
