package gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;

public class StreetAddress extends Address {

	private String street;
	
	private Integer number;

	public String getStreet() {
		return this.street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public Integer getNumber() {
		return this.number;
	}

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
	
	private boolean equals(StreetAddress that) {
		return super.equals(that) &&
			   Objects.equals(this.number, that.number) &&
			   Objects.equals(this.street, that.street);
	}
	
}
