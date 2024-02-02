package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

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
    
    Gender getGender();
    
    void setGender(Gender gender);
    
    Address getPrimaryAddress();
    
    void setPrimaryAddress(Address address);
    
    List<Address> getSecondaryAddresses();
    
    void setSecondaryAddresses(List<Address> secondaryAddresses);
    
}
