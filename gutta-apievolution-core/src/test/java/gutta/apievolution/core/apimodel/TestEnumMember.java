package gutta.apievolution.core.apimodel;

class TestEnumMember extends EnumMember<TestEnumType, TestEnumMember> {

    public TestEnumMember(String publicName, TestEnumType owner) {
        this(publicName, null, owner);
    }
    
    public TestEnumMember(String publicName, String internalName, TestEnumType owner) {
        super(publicName, internalName, owner);
    }

}
