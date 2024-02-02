package gutta.apievolution.inprocess.customerexample.consumer.objectmapping.v1;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;

public class Address {
	
    private String street;
    
    private Integer number;
    
    private Integer postalCode;
    
    private String city;

	public String getStreet() {
		return street;
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
	
    @Override
    public int hashCode() {
    	return (this.number + this.postalCode);
    }
	
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equals);
    }
    
    private boolean equals(Address that) {
    	return Objects.equals(this.number, that.number) &&
    		   Objects.equals(this.postalCode, that.postalCode) &&
    		   Objects.equals(this.street, that.street) &&
    		   Objects.equals(this.city, that.city);
    }

}
