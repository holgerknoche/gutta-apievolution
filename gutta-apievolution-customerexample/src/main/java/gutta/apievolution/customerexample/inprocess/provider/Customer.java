package gutta.apievolution.customerexample.inprocess.provider;

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
    
    Gender getGenderNew();
    
    void setGenderNew(Gender genderNew);
    
    Address getNewPrimaryAddress();
    
    void setNewPrimaryAddress(Address newPrimaryAddress);
    
    List<Address> getNewSecondaryAddresses();
    
    void setNewSecondaryAddresses(List<Address> newSecondaryAddresses);
    
    public StreetAddress getPrimaryAddress();
    
    void setPrimaryAddress(StreetAddress primaryAddress);
    
    List<StreetAddress> getSecondaryAddresses();
    
    void setSecondaryAddresses(List<StreetAddress> secondaryAddresses);
    
    Integer getGender();
    
    void setGender(Integer gender);

}
