package gutta.apievolution.inprocess.consumer.objectmapping;

import java.util.List;

public class ConsumerResult {

    private String resultField;

    private ConsumerEnum resultEnum;

    private List<ConsumerEnum> resultList;

    private ConsumerRecord resultRecord;

    public String getResultField() {
        return resultField;
    }

    public void setResultField(String resultField) {
        this.resultField = resultField;
    }

    public ConsumerEnum getResultEnum() {
        return resultEnum;
    }

    public void setResultEnum(ConsumerEnum resultEnum) {
        this.resultEnum = resultEnum;
    }

    public List<ConsumerEnum> getResultList() {
        return resultList;
    }

    public void setResultList(List<ConsumerEnum> resultList) {
        this.resultList = resultList;
    }

    public ConsumerRecord getResultRecord() {
        return resultRecord;
    }

    public void setResultRecord(ConsumerRecord resultRecord) {
        this.resultRecord = resultRecord;
    }

}
