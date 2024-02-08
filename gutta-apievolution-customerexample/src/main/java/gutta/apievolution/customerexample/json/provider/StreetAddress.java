package gutta.apievolution.customerexample.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeName;

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

}
