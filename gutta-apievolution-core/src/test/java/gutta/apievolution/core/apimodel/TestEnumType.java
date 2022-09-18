package gutta.apievolution.core.apimodel;

import java.util.Optional;

class TestEnumType extends EnumType<TestApiDefinition, TestEnumType, TestEnumMember> {

    public TestEnumType(String publicName, Optional<String> internalName, int typeId, TestApiDefinition owner) {
        super(publicName, internalName, typeId, owner);
    }

}
