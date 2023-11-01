package gutta.apievolution.core.apimodel;

interface TestUserDefinedType extends UserDefinedType<TestApiDefinition> {
    
    @Override
    default boolean isConsumerType() {
        return false;
    }
    
    @Override
    default boolean isProviderType() {
        return false;
    }
    
}
