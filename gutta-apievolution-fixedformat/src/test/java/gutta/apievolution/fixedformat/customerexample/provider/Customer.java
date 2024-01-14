package gutta.apievolution.fixedformat.customerexample.provider;

import java.util.List;

public class Customer {
    
    private String firstName;
    
    private String lastName;
    
    private String dateOfBirth;
    
    private int gender;
    
    private Gender genderNew;
    
    private Address primaryAddress;
    
    private List<Address> secondaryAddresses;

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

    public int getGender() {
        return this.gender;
    }

    public void setGender(int gender) {
        this.gender = gender;
    }

    public Gender getGenderNew() {
        return this.genderNew;
    }

    public void setGenderNew(Gender genderNew) {
        this.genderNew = genderNew;
    }

    public Address getPrimaryAddress() {
        return this.primaryAddress;
    }

    public void setPrimaryAddress(Address primaryAddress) {
        this.primaryAddress = primaryAddress;
    }

    public List<Address> getSecondaryAddresses() {
        return this.secondaryAddresses;
    }

    public void setSecondaryAddresses(List<Address> secondaryAddresses) {
        this.secondaryAddresses = secondaryAddresses;
    }        

}
