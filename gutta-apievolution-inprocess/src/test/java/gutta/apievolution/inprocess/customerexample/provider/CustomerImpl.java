package gutta.apievolution.inprocess.customerexample.provider;

import java.util.List;

public class CustomerImpl implements Customer {
	
    private String firstName;
    
    private String lastName;
    
    private String dateOfBirth;
        
    private Gender genderNew;
    
    private Address newPrimaryAddress;
    
    private List<Address> newSecondaryAddresses;
    
    private StreetAddress primaryAddress;
    
    private List<StreetAddress> secondaryAddresses;

    private Integer gender;

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
	public Gender getGenderNew() {
		return this.genderNew;
	}

    @Override
	public void setGenderNew(Gender genderNew) {
		this.genderNew = genderNew;
	}

    @Override
	public Address getNewPrimaryAddress() {
		return this.newPrimaryAddress;
	}

    @Override
	public void setNewPrimaryAddress(Address newPrimaryAddress) {
		this.newPrimaryAddress = newPrimaryAddress;
	}

    @Override
	public List<Address> getNewSecondaryAddresses() {
		return this.newSecondaryAddresses;
	}

    @Override
	public void setNewSecondaryAddresses(List<Address> newSecondaryAddresses) {
		this.newSecondaryAddresses = newSecondaryAddresses;
	}

    @Override
	public StreetAddress getPrimaryAddress() {
		return this.primaryAddress;
	}

    @Override
	public void setPrimaryAddress(StreetAddress primaryAddress) {
		this.primaryAddress = primaryAddress;
	}

    @Override
	public List<StreetAddress> getSecondaryAddresses() {
		return this.secondaryAddresses;
	}

    @Override
	public void setSecondaryAddresses(List<StreetAddress> secondaryAddresses) {
		this.secondaryAddresses = secondaryAddresses;
	}

    @Override
	public Integer getGender() {
		return this.gender;
	}

    @Override
	public void setGender(Integer gender) {
		this.gender = gender;
	}    

}
