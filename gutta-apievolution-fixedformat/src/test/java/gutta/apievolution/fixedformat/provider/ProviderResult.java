package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;

import java.util.List;

public class ProviderResult {

	@MaxLength(30)
	private String retField;
	
	private ProviderEnum resultEnum;
	
	@MaxLength(10)
	private List<ProviderEnum> resultList;
	
	public String getRetField() {
		return this.retField;
	}
	
	public void setRetField(String retField) {
		this.retField = retField;
	}
	
	public ProviderResult retField(String retField) {
		this.setRetField(retField);
		return this;
	}
	
	public ProviderEnum getResultEnum() {
		return this.resultEnum;
	}
	
	public void setResultEnum(ProviderEnum resultEnum) {
		this.resultEnum = resultEnum;
	}
	
	public ProviderResult resultEnum(ProviderEnum resultEnum) {
		this.setResultEnum(resultEnum);
		return this;
	}
	
	public List<ProviderEnum> getResultList() {
		return this.resultList;
	}
	
	public void setResultList(List<ProviderEnum> resultList) {
		this.resultList = resultList;
	}
	
	public ProviderResult resultList(List<ProviderEnum> resultList) {
		this.setResultEnum(resultEnum);
		return this;
	}
	
}
