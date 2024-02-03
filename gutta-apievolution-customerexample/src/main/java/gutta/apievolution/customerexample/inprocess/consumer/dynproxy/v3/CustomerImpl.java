package gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.List;
import java.util.Objects;

public class CustomerImpl implements Customer {

    private String firstName;
    
    private String lastName;
    
    private String dateOfBirth;
    
    private Integer gender;
    
    private Address primaryAddress;
    
    private List<Address> secondaryAddresses;

    @Override
	public String getFirstName() {
		return this.firstName;
	}

    @Override
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

    @Override
	public String getLastName() {
		return this.lastName;
	}

    @Override
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}
    
    @Override
    public String getDateOfBirth() {
        return this.dateOfBirth;
    }
    
    @Override
    public void setDateOfBirth(String dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
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
	public Address getPrimaryAddress() {
		return this.primaryAddress;
	}

    @Override
	public void setPrimaryAddress(Address primaryAddress) {
		this.primaryAddress = primaryAddress;
	}
    
    @Override
    public List<Address> getSecondaryAddresses() {
        return this.secondaryAddresses;
    }
    
    @Override
    public void setSecondaryAddresses(List<Address> secondaryAddresses) {
        this.secondaryAddresses = secondaryAddresses;
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
    		   Objects.equals(this.dateOfBirth, that.dateOfBirth) &&
    		   Objects.equals(this.primaryAddress, that.primaryAddress) &&
    		   Objects.equals(this.secondaryAddresses, that.secondaryAddresses);
    }
	
}
