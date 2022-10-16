package gutta.apievolution.core.apimodel;

import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;

class TestRecordType extends RecordType<TestApiDefinition, TestRecordType, TestField> implements TestUserDefinedType {
        
    public TestRecordType(String publicName, int typeId, TestApiDefinition owner) {
        this(publicName, noInternalName(), typeId, owner, Abstract.NO, RecordKind.RECORD, noSuperTypes());
    }
    
    public TestRecordType(String publicName, String internalName, int typeId, TestApiDefinition owner, 
            Abstract abstractFlag, RecordKind recordKind, Set<TestRecordType> superTypes) {
        
        super(publicName, internalName, typeId, owner, abstractFlag, recordKind, superTypes);
    }
    
}
