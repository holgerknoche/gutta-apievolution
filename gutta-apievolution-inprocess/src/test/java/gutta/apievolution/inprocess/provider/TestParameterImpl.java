package gutta.apievolution.inprocess.provider;

import java.util.List;

public class TestParameterImpl implements TestParameter {

    private String fieldA;

    private String field2;

    private ProviderEnum testEnum;

    private List<ProviderEnum> testList;

    private ProviderRecord testRecord;

    public String getFieldA() {
        return this.fieldA;
    }

    public void setFieldA(String fieldA) {
        this.fieldA = fieldA;
    }

    public String getField2() {
        return this.field2;
    }

    public void setField2(String field2) {
        this.field2 = field2;
    }

    public ProviderEnum getTestEnum() {
        return this.testEnum;
    }

    public void setTestEnum(ProviderEnum testEnum) {
        this.testEnum = testEnum;
    }

    public List<ProviderEnum> getTestList() {
        return this.testList;
    }

    public void setTestList(List<ProviderEnum> testList) {
        this.testList = testList;
    }

    public ProviderRecord getTestRecord() {
        return this.testRecord;
    }

    public void setTestRecord(ProviderRecord testRecord) {
        this.testRecord = testRecord;
    }

}
