package gutta.apievolution.benchmarks.json.provider;

import gutta.apievolution.core.apimodel.provider.RevisionHistory;

import java.util.Set;

public class TestMethod25ProviderProxy extends ProviderProxyTemplate<ProviderResult25> {

    private static final ProviderResult25 RESULT = createResult(); 
    
    public static ProviderResult25 createResult() {
        ProviderResult25 result = new ProviderResult25();
        
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
        result.setIntField11(11);
        result.setIntField12(12);
        result.setIntField13(13);
        result.setIntField14(14);
        result.setIntField15(15);
        result.setIntField16(16);
        result.setIntField17(17);
        result.setIntField18(18);
        result.setIntField19(19);
        result.setIntField20(20);
        result.setIntField21(21);
        result.setIntField22(22);
        result.setIntField23(23);
        result.setIntField24(24);
        result.setIntField25(25);

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
        result.setStringField11("11");
        result.setStringField12("12");
        result.setStringField13("13");
        result.setStringField14("14");
        result.setStringField15("15");
        result.setStringField16("16");
        result.setStringField17("17");
        result.setStringField18("18");
        result.setStringField19("19");
        result.setStringField20("20");
        result.setStringField21("21");
        result.setStringField22("22");
        result.setStringField23("23");
        result.setStringField24("24");
        result.setStringField25("25");
        
        return result;
    }
    
    public TestMethod25ProviderProxy(RevisionHistory revisionHistory, Set<Integer> supportedRevisions) {
        super("testMethod25", revisionHistory, supportedRevisions, "ProviderResult25");
    }
    
    @Override
    protected ProviderResult25 invokeOperation(ProviderParameter parameter) {
        return RESULT;
    }
    
}
