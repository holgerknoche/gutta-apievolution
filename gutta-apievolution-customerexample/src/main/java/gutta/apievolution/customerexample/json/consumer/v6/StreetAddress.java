package gutta.apievolution.customerexample.json.consumer.v6;

import com.fasterxml.jackson.annotation.JsonTypeName;
import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

@JsonTypeName("StreetAddress")
public class StreetAddress extends Address {

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
