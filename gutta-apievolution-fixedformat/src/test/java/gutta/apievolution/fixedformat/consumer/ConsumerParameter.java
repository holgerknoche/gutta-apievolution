package gutta.apievolution.fixedformat.consumer;

import java.util.List;

import gutta.apievolution.fixedformat.MaxLength;

public class ConsumerParameter {

    @MaxLength(30)
    private String testField;
    
    private ConsumerEnum testEnum;
    
    @MaxLength(10)
    private List<ConsumerEnum> testList;
    
    public String getTestField() {
        return this.testField;
    }
    
    public void setTestField(String testField) {
        this.testField = testField;
    }
    
    public ConsumerParameter testField(String testField) {
        this.setTestField(testField);
        return this;
    }
    
    public ConsumerEnum getTestEnum() {
        return this.testEnum;
    }
    
    public void setTestEnum(ConsumerEnum testEnum) {
        this.testEnum = testEnum;
    }
    
    public ConsumerParameter testEnum(ConsumerEnum testEnum) {
        this.setTestEnum(testEnum);
        return this;
    }
    
    public List<ConsumerEnum> getTestList() {
        return this.testList;
    }
    
    public void setTestList(List<ConsumerEnum> testList) {
        this.testList = testList;
    }
    
    public ConsumerParameter testList(List<ConsumerEnum> testList) {
        this.setTestList(testList);
        return this;
    }
    
}
