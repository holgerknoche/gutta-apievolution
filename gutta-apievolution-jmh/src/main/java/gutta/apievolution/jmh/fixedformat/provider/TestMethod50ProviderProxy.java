package gutta.apievolution.jmh.fixedformat.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod50ProviderProxy extends ProviderProxyTemplate<ProviderResult50> {

    private static final ProviderResult50 RESULT = createResult();

    public static ProviderResult50 createResult() {
        ProviderResult50 result = new ProviderResult50();

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
        result.setIntField26(26);
        result.setIntField27(27);
        result.setIntField28(28);
        result.setIntField29(29);
        result.setIntField30(30);
        result.setIntField31(31);
        result.setIntField32(32);
        result.setIntField33(33);
        result.setIntField34(34);
        result.setIntField35(35);
        result.setIntField36(36);
        result.setIntField37(37);
        result.setIntField38(38);
        result.setIntField39(39);
        result.setIntField40(40);
        result.setIntField41(41);
        result.setIntField42(42);
        result.setIntField43(43);
        result.setIntField44(44);
        result.setIntField45(45);
        result.setIntField46(46);
        result.setIntField47(47);
        result.setIntField48(48);
        result.setIntField49(49);
        result.setIntField50(50);

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
        result.setStringField26("26");
        result.setStringField27("27");
        result.setStringField28("28");
        result.setStringField29("29");
        result.setStringField30("30");
        result.setStringField31("31");
        result.setStringField32("32");
        result.setStringField33("33");
        result.setStringField34("34");
        result.setStringField35("35");
        result.setStringField36("36");
        result.setStringField37("37");
        result.setStringField38("38");
        result.setStringField39("39");
        result.setStringField40("40");
        result.setStringField41("41");
        result.setStringField42("42");
        result.setStringField43("43");
        result.setStringField44("44");
        result.setStringField45("45");
        result.setStringField46("46");
        result.setStringField47("47");
        result.setStringField48("48");
        result.setStringField49("49");
        result.setStringField50("50");

        return result;
    }

    public TestMethod50ProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper,
            Charset charset) {

        super("testMethod50", ProviderResult50.class, consumerToProviderScript, providerToConsumerScript, mapper, charset);
    }

    @Override
    protected ProviderResult50 invokeOperation(ProviderParameter parameter) {
        return RESULT;
    }

}
