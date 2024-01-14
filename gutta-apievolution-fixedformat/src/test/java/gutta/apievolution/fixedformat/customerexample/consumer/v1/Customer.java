package gutta.apievolution.fixedformat.customerexample.consumer.v1;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;

public class Customer {
    
    @MaxLength(20)
    private String firstName;
    
    @MaxLength(20)
    private String lastName;
    
    private int gender;
    
    private Address address;

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

    public int getGender() {
        return this.gender;
    }

    public void setGender(int gender) {
        this.gender = gender;
    }

    public Address getAddress() {
        return this.address;
    }

    public void setAddress(Address address) {
        this.address = address;
    }       

}
