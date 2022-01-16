package gutta.apievolution.json.consumer;

public class ConsumerResult {

    private String resultField;

    private ConsumerEnum resultEnum;

    public String getResultField() {
        return this.resultField;
    }

    public void setResultField(String resultField) {
        this.resultField = resultField;
    }

    public ConsumerEnum getResultEnum() {
        return this.resultEnum;
    }

    public void setResultEnum(ConsumerEnum resultEnum) {
        this.resultEnum = resultEnum;
    }

}
