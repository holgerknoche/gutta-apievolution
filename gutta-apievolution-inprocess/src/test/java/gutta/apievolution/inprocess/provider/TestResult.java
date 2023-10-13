package gutta.apievolution.inprocess.provider;

import java.util.List;

public class TestResult {

	private String resultField;

	private ProviderEnum resultEnum;

	private List<ProviderEnum> resultList;

	public String getResultField() {
		return resultField;
	}

	public void setResultField(String resultField) {
		this.resultField = resultField;
	}

	public ProviderEnum getResultEnum() {
		return resultEnum;
	}

	public void setResultEnum(ProviderEnum resultEnum) {
		this.resultEnum = resultEnum;
	}

	public List<ProviderEnum> getResultList() {
		return resultList;
	}

	public void setResultList(List<ProviderEnum> resultList) {
		this.resultList = resultList;
	}

}
