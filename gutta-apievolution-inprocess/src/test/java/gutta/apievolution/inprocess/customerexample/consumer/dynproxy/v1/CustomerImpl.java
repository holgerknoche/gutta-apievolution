package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v1;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;

public class CustomerImpl implements Customer {

    private String firstName;
    
    private String lastName;
    
    private Integer gender;
    
    private Address address;

    @Override
	public String getFirstName() {
		return firstName;
	}

    @Override
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

    @Override
	public String getLastName() {
		return lastName;
	}

    @Override
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

    @Override
	public Integer getGender() {
		return this.gender;
	}

    @Override
	public void setGender(Integer gender) {
		this.gender = gender;
	}

    @Override
	public Address getAddress() {
		return this.address;
	}

    @Override
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
    
    private boolean equals(CustomerImpl that) {
    	return Objects.equals(this.gender, that.gender) &&
    		   Objects.equals(this.firstName, that.firstName) &&
    		   Objects.equals(this.lastName, that.lastName) &&
    		   Objects.equals(this.address, that.address);
    }
	
}
