package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.UserDefinedType;

/**
 * Interface for provider-specific UDTs.
 */
public interface ProviderUserDefinedType extends ProviderApiDefinitionElement, UserDefinedType<ProviderApiDefinition> {
    
    @Override
    default boolean isConsumerType() {
        return false;
    }
    
    @Override
    default boolean isProviderType() {
        return true;
    }
    
}
