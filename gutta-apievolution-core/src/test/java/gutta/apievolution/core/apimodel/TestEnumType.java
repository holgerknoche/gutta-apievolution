package gutta.apievolution.core.apimodel;

class TestEnumType extends EnumType<TestApiDefinition, TestEnumType, TestEnumMember> implements TestUserDefinedType {

    public TestEnumType(String publicName, int typeId, TestApiDefinition owner) {
        this(publicName, null, typeId, owner);
    }
    
    public TestEnumType(String publicName, String internalName, int typeId, TestApiDefinition owner) {
        super(publicName, internalName, typeId, owner);
    }

}
