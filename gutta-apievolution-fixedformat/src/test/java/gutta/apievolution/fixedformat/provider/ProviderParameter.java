package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;

import java.util.List;

public class ProviderParameter {

	@MaxLength(30)
	private String fieldA;
	
	@MaxLength(30)
	private String field2;
	
	private ProviderEnum testEnum;
	
	@MaxLength(10)
	private List<ProviderEnum> testList;
	
	public String getFieldA() {
		return this.fieldA;
	}
	
	public void setFieldA(String fieldA) {
		this.fieldA = fieldA;
	}
	
	public ProviderParameter fieldA(String fieldA) {
		this.setFieldA(fieldA);
		return this;
	}
	
	public String getField2() {
		return this.field2;
	}
	
	public void setField2(String field2) {
		this.field2 = field2;
	}
	
	public ProviderParameter field2(String field2) {
		this.setField2(field2);
		return this;
	}
	
	public ProviderEnum getTestEnum() {
		return this.testEnum;
	}
	
	public void setTestEnum(ProviderEnum testEnum) {
		this.testEnum = testEnum;
	}
	
	public ProviderParameter testEnum(ProviderEnum testEnum) {
		this.setTestEnum(testEnum);
		return this;
	}
	
}
