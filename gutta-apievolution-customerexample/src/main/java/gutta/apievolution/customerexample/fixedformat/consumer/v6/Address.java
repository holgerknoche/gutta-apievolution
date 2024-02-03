package gutta.apievolution.customerexample.fixedformat.consumer.v6;

import java.util.Objects;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.SubTypes;

@SubTypes({StreetAddress.class, POBoxAddress.class})
public abstract class Address {

	private int postalCode;
	
	@MaxLength(20)
	private String city;

	public int getPostalCode() {
		return this.postalCode;
	}

	public void setPostalCode(int postalCode) {
		this.postalCode = postalCode;
	}

	public String getCity() {
		return this.city;
	}

	public void setCity(String city) {
		this.city = city;
	}
	
	protected boolean equals(Address that) {
		return (this.postalCode == that.postalCode) &&
			   Objects.equals(this.city, that.city);
	}
	
}
