package gutta.apievolution.customerexample.json.consumer.v6;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

import java.util.Objects;

@JsonTypeInfo(use = Id.NAME)
@JsonSubTypes({@Type(StreetAddress.class), @Type(POBoxAddress.class)})
public abstract class Address {

	private int postalCode;
	
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
