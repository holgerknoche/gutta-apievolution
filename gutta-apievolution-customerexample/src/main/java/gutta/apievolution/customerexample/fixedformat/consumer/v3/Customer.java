package gutta.apievolution.customerexample.fixedformat.consumer.v3;

import java.util.List;
import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;
import gutta.apievolution.fixedformat.objectmapping.MaxLength;

public class Customer {

	@MaxLength(20)
    private String firstName;
    
	@MaxLength(20)
    private String lastName;
    
	@MaxLength(10)
    private String dateOfBirth;
    
    private int gender;
    
    private Address primaryAddress;
    
    @MaxLength(10)
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
    
    @Override
    public int hashCode() {
    	return Objects.hash(this.firstName, this.lastName);
    }
    
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equals);
    }
    
    private boolean equals(Customer that) {
    	return (this.gender == that.gender) &&
    		   Objects.equals(this.firstName, that.firstName) &&
    		   Objects.equals(this.lastName, that.lastName) &&
    		   Objects.equals(this.dateOfBirth, that.dateOfBirth) &&
    		   Objects.equals(this.primaryAddress, that.primaryAddress) &&
    		   Objects.equals(this.secondaryAddresses, that.secondaryAddresses);
    }

}
