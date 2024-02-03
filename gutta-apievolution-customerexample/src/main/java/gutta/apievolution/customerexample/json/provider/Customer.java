package gutta.apievolution.customerexample.json.provider;

import java.util.List;

public class Customer {
    
    private String firstName;
    
    private String lastName;
    
    private String dateOfBirth;
        
    private Gender genderNew;
    
    private Address newPrimaryAddress;
    
    private List<Address> newSecondaryAddresses;
    
    private StreetAddress primaryAddress;
    
    private List<StreetAddress> secondaryAddresses;

    private Integer gender;    
    
    public String getFirstName() {
        return this.firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return this.lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getDateOfBirth() {
        return this.dateOfBirth;
    }

    public void setDateOfBirth(String dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    public Integer getGender() {
        return this.gender;
    }

    public void setGender(Integer gender) {
        this.gender = gender;
    }

    public Gender getGenderNew() {
        return this.genderNew;
    }

    public void setGenderNew(Gender genderNew) {
        this.genderNew = genderNew;
    }

    public StreetAddress getPrimaryAddress() {
        return this.primaryAddress;
    }
    
    public void setPrimaryAddress(StreetAddress primaryAddress) {
        this.primaryAddress = primaryAddress;
    }
    
    public List<StreetAddress> getSecondaryAddresses() {
        return this.secondaryAddresses;
    }
    
    public void setSecondaryAddresses(List<StreetAddress> secondaryAddresses) {
        this.secondaryAddresses = secondaryAddresses;
    }
    
    public Address getNewPrimaryAddress() {
        return this.newPrimaryAddress;
    }

    public void setNewPrimaryAddress(Address newPrimaryAddress) {
        this.newPrimaryAddress = newPrimaryAddress;
    }

    public List<Address> getNewSecondaryAddresses() {
        return this.newSecondaryAddresses;
    }

    public void setNewSecondaryAddresses(List<Address> newSecondaryAddresses) {
        this.newSecondaryAddresses = newSecondaryAddresses;
    }        

}
