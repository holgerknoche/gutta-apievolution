package gutta.apievolution.customerexample.fixedformat.provider;

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

}
