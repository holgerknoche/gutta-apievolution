package gutta.apievolution.inprocess.customerexample.provider;

public class StreetAddressImpl extends AddressImpl implements StreetAddress {

    private String street;
    
    private Integer number;

    @Override
	public String getStreet() {
		return street;
	}

    @Override
	public void setStreet(String street) {
		this.street = street;
	}

    @Override
	public Integer getNumber() {
		return number;
	}

    @Override
	public void setNumber(Integer number) {
		this.number = number;
	}
	
}
