package gutta.apievolution.inprocess.customerexample.provider;

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
	
}
