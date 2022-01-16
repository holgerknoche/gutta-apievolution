package gutta.apievolution.json.provider;

public class ProviderResult {

    private String retField;

    private ProviderEnum resultEnum;

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
}
