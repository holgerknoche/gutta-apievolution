package gutta.apievolution.core.apimodel;

import java.util.Optional;

class TestField extends Field<TestRecordType, TestField> {

    public TestField(String publicName, TestRecordType owner, Type type) {
        this(publicName, Optional.empty(), owner, type, Optionality.OPT_IN, false);
    }
    
    public TestField(String publicName, Optional<String> internalName, TestRecordType owner, Type type,
            Optionality optionality, boolean inherited) {
        
        super(publicName, internalName, owner, type, optionality, inherited);
    }

}
