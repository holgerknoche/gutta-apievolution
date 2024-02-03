package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;

public class StreetAddressImpl extends AddressImpl implements StreetAddress {

	private String street;
	
	private Integer number;

	@Override
	public String getStreet() {
		return this.street;
	}

	@Override
	public void setStreet(String street) {
		this.street = street;
	}

	@Override
	public Integer getNumber() {
		return this.number;
	}

	@Override
	public void setNumber(Integer number) {
		this.number = number;
	}
	
	@Override
	public int hashCode() {
		return (this.getPostalCode() + this.number);
	}
	
	public boolean equals(Object that) {
		return EqualityUtil.equals(this, that, this::equals);
	}
	
	private boolean equals(StreetAddressImpl that) {
		return super.equals(that) &&
			   Objects.equals(this.number, that.number) &&
			   Objects.equals(this.street, that.street);
	}
	
}
