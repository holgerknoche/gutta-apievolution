package gutta.apievolution.json.customerexample.consumer.v1;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

public class Address {
    
    private String street;
    
    private int number;
    
    private int postalCode;
    
    private String city;

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
    
    @Override
    public int hashCode() {
    	return (this.number + this.postalCode);
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equals);
    }
    
    private boolean equals(Address that) {
    	return (this.number == that.number) &&
    		   (this.postalCode == that.postalCode) &&
    		   Objects.equals(this.street, that.street) &&
    		   Objects.equals(this.city, that.city);
    }

}
