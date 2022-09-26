package gutta.apievolution.core.apimodel;

import java.util.Optional;

class TestEnumType extends EnumType<TestApiDefinition, TestEnumType, TestEnumMember> implements TestUserDefinedType {

    public TestEnumType(String publicName, int typeId, TestApiDefinition owner) {
        this(publicName, Optional.empty(), typeId, owner);
    }
    
    public TestEnumType(String publicName, Optional<String> internalName, int typeId, TestApiDefinition owner) {
        super(publicName, internalName, typeId, owner);
    }

}
