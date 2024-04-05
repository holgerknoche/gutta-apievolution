package gutta.apievolution.fixedformat.consumer;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

import java.util.List;

@TypeId(2)
public class ConsumerResult {
	
	private ConsumerEnum resultEnum;
	
	@MaxLength(30)
	private String resultField;
	
	@MaxLength(10)
	private List<ConsumerEnum> resultList;
	
	public ConsumerEnum getResultEnum() {
		return this.resultEnum;
	}
	
	public void setResultEnum(ConsumerEnum resultEnum) {
		this.resultEnum = resultEnum;
	}
	
	public String getResultField() {
		return this.resultField;
	}
	
	public void setResultField(String resultField) {
		this.resultField = resultField;
	}
	
	public List<ConsumerEnum> getResultList() {
		return this.resultList;
	}
	
	public void setResultList(List<ConsumerEnum> resultList) {
		this.resultList = resultList;
	}

}
