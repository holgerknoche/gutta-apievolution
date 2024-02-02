package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

import java.util.Objects;

public abstract class AddressImpl implements Address {

	private Integer postalCode;
	
	private String city;

	@Override
	public Integer getPostalCode() {
		return this.postalCode;
	}

	@Override
	public void setPostalCode(Integer postalCode) {
		this.postalCode = postalCode;
	}

	@Override
	public String getCity() {
		return this.city;
	}

	@Override
	public void setCity(String city) {
		this.city = city;
	}
	
	protected boolean equals(AddressImpl that) {
		return Objects.equals(this.postalCode, that.postalCode) &&
			   Objects.equals(this.city, that.city);
	}
	
}
