package gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3;

import gutta.apievolution.inprocess.ImplementedBy;

import java.util.List;

@ImplementedBy(CustomerImpl.class)
public interface Customer {

    String getFirstName();
    
    void setFirstName(String firstName);
    
    String getLastName();
    
    void setLastName(String lastName);
    
    String getDateOfBirth();
    
    void setDateOfBirth(String dateOfBirth);
    
    Integer getGender();
    
    void setGender(Integer gender);
    
    Address getPrimaryAddress();
    
    void setPrimaryAddress(Address address);
    
    List<Address> getSecondaryAddresses();
    
    void setSecondaryAddresses(List<Address> secondaryAddresses);
    
}
