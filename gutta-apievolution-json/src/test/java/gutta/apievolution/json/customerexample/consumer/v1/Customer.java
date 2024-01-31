package gutta.apievolution.json.customerexample.consumer.v1;

import java.util.Objects;

public class Customer {
    
    private String firstName;
    
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
    
    @Override
    public int hashCode() {
    	return Objects.hash(this.firstName, this.lastName);
    }
    
    @Override
    public boolean equals(Object that) {
    	if (this == that) {
    		return true;
    	} else if (that != null && this.getClass() == that.getClass()) {
    		return this.equals((Customer) that);
    	} else {
    		return false;
    	}
    }
    
    private boolean equals(Customer that) {
    	return (this.gender == that.gender) &&
    		   Objects.equals(this.firstName, that.firstName) &&
    		   Objects.equals(this.lastName, that.lastName) &&
    		   Objects.equals(this.address, that.address);
    }

}
