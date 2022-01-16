package gutta.apievolution.json.consumer;

import java.util.List;

public class ConsumerParameter {

    private String testField;

    private ConsumerEnum testEnum;

    private List<ConsumerEnum> testList;

    public String getTestField() {
        return this.testField;
    }

    public void setTestField(String testField) {
        this.testField = testField;
    }

    public ConsumerEnum getTestEnum() {
        return this.testEnum;
    }

    public void setTestEnum(ConsumerEnum testEnum) {
        this.testEnum = testEnum;
    }

    public List<ConsumerEnum> getTestList() {
        return this.testList;
    }

    public void setTestList(List<ConsumerEnum> testList) {
        this.testList = testList;
    }
}
