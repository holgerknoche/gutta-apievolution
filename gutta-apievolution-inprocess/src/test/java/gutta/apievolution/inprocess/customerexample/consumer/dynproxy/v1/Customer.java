package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v1;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(CustomerImpl.class)
public interface Customer {

    String getFirstName();
    
    void setFirstName(String firstName);
    
    String getLastName();
    
    void setLastName(String lastName);
    
    Integer getGender();
    
    void setGender(Integer gender);
    
    Address getAddress();
    
    void setAddress(Address address);
    
}
