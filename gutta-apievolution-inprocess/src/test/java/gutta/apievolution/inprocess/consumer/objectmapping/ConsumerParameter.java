package gutta.apievolution.inprocess.consumer.objectmapping;

import java.util.List;

public class ConsumerParameter {

    private String testField;

    private ConsumerEnum testEnum;

    private List<ConsumerEnum> testList;

    private ConsumerRecord testRecord;

    public String getTestField() {
        return testField;
    }

    public void setTestField(String testField) {
        this.testField = testField;
    }

    public ConsumerEnum getTestEnum() {
        return testEnum;
    }

    public void setTestEnum(ConsumerEnum testEnum) {
        this.testEnum = testEnum;
    }

    public List<ConsumerEnum> getTestList() {
        return testList;
    }

    public void setTestList(List<ConsumerEnum> testList) {
        this.testList = testList;
    }

    public ConsumerRecord getTestRecord() {
        return testRecord;
    }

    public void setTestRecord(ConsumerRecord testRecord) {
        this.testRecord = testRecord;
    }

}
