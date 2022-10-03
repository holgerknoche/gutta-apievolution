package gutta.apievolution.core.apimodel;

class TestField extends Field<TestRecordType, TestField> {

    public TestField(String publicName, TestRecordType owner, Type type) {
        this(publicName, null, owner, type, Optionality.OPT_IN, false);
    }
    
    public TestField(String publicName, String internalName, TestRecordType owner, Type type,
            Optionality optionality, boolean inherited) {
        
        super(publicName, internalName, owner, type, optionality, inherited);
    }

}
