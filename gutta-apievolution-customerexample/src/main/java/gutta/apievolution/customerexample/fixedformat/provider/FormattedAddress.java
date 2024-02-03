package gutta.apievolution.customerexample.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;

public class FormattedAddress {
    
    @MaxLength(40)
    private String address;

    public String getAddress() {
        return this.address;
    }

    public void setAddress(String address) {
        this.address = address;
    }        

}
