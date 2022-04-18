package gutta.apievolution.fixedformat.consumer;

import java.util.List;

public class ConsumerResult {
	
	private ConsumerEnum resultEnum;
	
	private String resultField;
	
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
