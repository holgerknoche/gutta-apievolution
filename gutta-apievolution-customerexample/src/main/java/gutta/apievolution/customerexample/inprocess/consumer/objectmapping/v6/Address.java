package gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6;

import java.util.Objects;

public abstract class Address {

	private Integer postalCode;
	
	private String city;

	public Integer getPostalCode() {
		return this.postalCode;
	}

	public void setPostalCode(Integer postalCode) {
		this.postalCode = postalCode;
	}

	public String getCity() {
		return this.city;
	}

	public void setCity(String city) {
		this.city = city;
	}
	
	protected boolean equals(Address that) {
		return Objects.equals(this.postalCode, that.postalCode) &&
			   Objects.equals(this.city, that.city);
	}
	
}
