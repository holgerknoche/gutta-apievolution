package gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v1;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;

public class Customer {

    private String firstName;
    
    private String lastName;
    
    private Integer gender;
    
    private Address address;

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public Integer getGender() {
		return this.gender;
	}

	public void setGender(Integer gender) {
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
    	return EqualityUtil.equals(this, that, this::equals);
    }
    
    private boolean equals(Customer that) {
    	return Objects.equals(this.gender, that.gender) &&
    		   Objects.equals(this.firstName, that.firstName) &&
    		   Objects.equals(this.lastName, that.lastName) &&
    		   Objects.equals(this.address, that.address);
    }
	
}
