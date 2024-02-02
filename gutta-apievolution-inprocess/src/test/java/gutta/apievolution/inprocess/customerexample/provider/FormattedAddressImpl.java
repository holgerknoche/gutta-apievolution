package gutta.apievolution.inprocess.customerexample.provider;

public class FormattedAddressImpl implements FormattedAddress {

    private String address;

    @Override
	public String getAddress() {
		return address;
	}

    @Override
	public void setAddress(String address) {
		this.address = address;
	}
	
}
