package gutta.apievolution.core.apimodel;

import java.util.Optional;

class TestEnumMember extends EnumMember<TestEnumType, TestEnumMember> {

    public TestEnumMember(String publicName, Optional<String> internalName, TestEnumType owner) {
        super(publicName, internalName, owner);
    }

}
