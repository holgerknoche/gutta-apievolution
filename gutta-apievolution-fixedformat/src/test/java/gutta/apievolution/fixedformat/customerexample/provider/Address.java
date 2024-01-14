package gutta.apievolution.fixedformat.customerexample.provider;

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

}
