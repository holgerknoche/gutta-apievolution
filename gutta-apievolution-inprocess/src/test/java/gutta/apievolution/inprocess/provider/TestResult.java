package gutta.apievolution.inprocess.provider;

import java.util.List;

public class TestResult {

    private String retField;

    private ProviderEnum resultEnum;

    private List<ProviderEnum> resultList;

    private ProviderRecord resultRecord;

    public String getRetField() {
        return retField;
    }

    public void setRetField(String retField) {
        this.retField = retField;
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

    public ProviderRecord getResultRecord() {
        return resultRecord;
    }

    public void setResultRecord(ProviderRecord resultRecord) {
        this.resultRecord = resultRecord;
    }

}
