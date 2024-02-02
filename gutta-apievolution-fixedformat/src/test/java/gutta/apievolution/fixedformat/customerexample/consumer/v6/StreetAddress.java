package gutta.apievolution.fixedformat.customerexample.consumer.v6;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;
import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(3)
public class StreetAddress extends Address {

	@MaxLength(20)
	private String street;
	
	private int number;

	public String getStreet() {
		return this.street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public int getNumber() {
		return this.number;
	}

	public void setNumber(int number) {
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
			   (this.number == that.number) &&
			   Objects.equals(this.street, that.street);
	}
	
}
