package gutta.apievolution.core.apimodel;

import java.util.Optional;

class TestRecordType extends RecordType<TestApiDefinition, TestRecordType, TestField> implements TestUserDefinedType {
        
    public TestRecordType(String publicName, int typeId, TestApiDefinition owner) {
        this(publicName, Optional.empty(), typeId, owner, false, false, Optional.empty());
    }
    
    public TestRecordType(String publicName, Optional<String> internalName, int typeId, TestApiDefinition owner, 
            boolean abstractFlag, boolean exception, Optional<TestRecordType> superType) {
        
        super(publicName, internalName, typeId, owner, abstractFlag, exception, superType);
    }
    
}
