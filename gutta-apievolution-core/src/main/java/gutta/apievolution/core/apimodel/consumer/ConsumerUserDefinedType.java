package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.UserDefinedType;

/**
 * Interface for consumer-specific UDTs.
 */
public interface ConsumerUserDefinedType extends ConsumerApiDefinitionElement, UserDefinedType<ConsumerApiDefinition> {
    
    @Override
    default boolean isConsumerType() {
        return true;
    }
    
    @Override
    default boolean isProviderType() {
        return false;
    }
    
}
