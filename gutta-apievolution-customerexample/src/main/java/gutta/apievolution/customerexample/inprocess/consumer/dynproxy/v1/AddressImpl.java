package gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v1;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;

public class AddressImpl implements Address {
	
    private String street;
    
    private Integer number;
    
    private Integer postalCode;
    
    private String city;

    @Override
	public String getStreet() {
		return street;
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
	
    @Override
    public int hashCode() {
    	return (this.number + this.postalCode);
    }
	
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equals);
    }
    
    private boolean equals(AddressImpl that) {
    	return Objects.equals(this.number, that.number) &&
    		   Objects.equals(this.postalCode, that.postalCode) &&
    		   Objects.equals(this.street, that.street) &&
    		   Objects.equals(this.city, that.city);
    }

}
