package gutta.apievolution.json.customerexample.consumer.v6;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.List;
import java.util.Objects;

public class Customer {
	
	private String firstName;
	
	private String lastName;
	
	private String dateOfBirth;
	
	private Gender gender;
	
	private Address primaryAddress;
	
	private List<Address> secondaryAddresses;

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

	public String getDateOfBirth() {
		return dateOfBirth;
	}

	public void setDateOfBirth(String dateOfBirth) {
		this.dateOfBirth = dateOfBirth;
	}

	public Gender getGender() {
		return gender;
	}

	public void setGender(Gender gender) {
		this.gender = gender;
	}

	public Address getPrimaryAddress() {
		return primaryAddress;
	}

	public void setPrimaryAddress(Address primaryAddress) {
		this.primaryAddress = primaryAddress;
	}

	public List<Address> getSecondaryAddresses() {
		return secondaryAddresses;
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
