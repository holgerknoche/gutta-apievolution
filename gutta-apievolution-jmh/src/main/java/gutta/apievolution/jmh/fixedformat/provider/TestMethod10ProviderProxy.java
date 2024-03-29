package gutta.apievolution.jmh.fixedformat.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

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

    public TestMethod10ProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper,
            Charset charset) {

        super("testMethod10", ProviderResult10.class, consumerToProviderScript, providerToConsumerScript, mapper, charset);
    }

    @Override
    protected ProviderResult10 invokeOperation(ProviderParameter parameter) {
        return RESULT;
    }

}
