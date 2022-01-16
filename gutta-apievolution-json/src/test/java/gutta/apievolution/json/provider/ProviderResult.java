package gutta.apievolution.json.provider;

import java.util.List;

public class ProviderResult {

    private String retField;

    private ProviderEnum resultEnum;

    private List<ProviderEnum> resultList;

    public String getRetField() {
        return this.retField;
    }

    public void setRetField(String retField) {
        this.retField = retField;
    }

    public ProviderEnum getResultEnum() {
        return this.resultEnum;
    }

    public void setResultEnum(ProviderEnum resultEnum) {
        this.resultEnum = resultEnum;
    }

    public List<ProviderEnum> getResultList() {
        return this.resultList;
    }

    public void setResultList(List<ProviderEnum> resultList) {
        this.resultList = resultList;
    }
}
