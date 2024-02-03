package gutta.apievolution.fixedformat.customerexample.consumer.v3;

import java.util.Objects;

import gutta.apievolution.core.util.EqualityUtil;
import gutta.apievolution.fixedformat.objectmapping.MaxLength;

public class Address {

	@MaxLength(20)
    private String street;
    
    private int number;
    
    private int postalCode;
    
    @MaxLength(20)
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
    
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equals);
    }
    
    private boolean equals(Address that) {
    	return (this.number == that.number) &&
    		   (this.postalCode == that.postalCode) &&
    		   Objects.equals(this.city, that.city) &&
    		   Objects.equals(this.street, that.street);
    }
    
}
