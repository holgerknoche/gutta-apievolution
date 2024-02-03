package gutta.apievolution.jmh.json.provider;

import gutta.apievolution.core.apimodel.provider.RevisionHistory;

import java.util.Set;

public class TestMethod10ProviderProxy extends ProviderProxyTemplate<ProviderResult10> {

    private static final ProviderResult10 RESULT = createResult(); 
    
    public static ProviderResult10 createResult() {
        ProviderResult10 result = new ProviderResult10();

        result.setIntField1(1);
        result.setIntField2(2);
        result.setIntField3(3);
        result.setIntField4(4);
        result.setIntField5(5);
        result.setIntField6(6);
        result.setIntField7(7);
        result.setIntField8(8);
        result.setIntField9(9);
        result.setIntField10(10);

        result.setStringField1("1");
        result.setStringField2("2");
        result.setStringField3("3");
        result.setStringField4("4");
        result.setStringField5("5");
        result.setStringField6("6");
        result.setStringField7("7");
        result.setStringField8("8");
        result.setStringField9("9");
        result.setStringField10("10");

        return result;
    }
    
    public TestMethod10ProviderProxy(RevisionHistory revisionHistory, Set<Integer> supportedRevisions) {
        super("testMethod10", revisionHistory, supportedRevisions, "Result10");
    }
    
    @Override
    protected ProviderResult10 invokeOperation(ProviderParameter parameter) {
        return RESULT;
    }
    
}
