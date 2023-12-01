package gutta.apievolution.inprocess.provider;

import gutta.apievolution.inprocess.ImplementedBy;

import java.util.List;

@ImplementedBy(TestParameterImpl.class)
public interface TestParameter {

    String getFieldA();

    String getField2();

    ProviderEnum getTestEnum();

    List<ProviderEnum> getTestList();

    ProviderRecord getTestRecord();

}
