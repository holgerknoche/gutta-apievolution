package gutta.apievolution.json.consumer;

import java.util.List;

public class ConsumerResult {

    private String resultField;

    private ConsumerEnum resultEnum;

    private List<ConsumerEnum> resultList;

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

    public List<ConsumerEnum> getResultList() {
        return this.resultList;
    }

    public void setResultList(List<ConsumerEnum> resultList) {
        this.resultList = resultList;
    }
}
