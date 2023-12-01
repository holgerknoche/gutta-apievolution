package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;

/**
 * A {@link UDTToClassMap} represents a map of the user-defined types of an API definition to the classes that represent them.
 */
public interface UDTToClassMap {

    /**
     * Maps a consumer type to the class that represents it.
     * 
     * @param <T>  The type of the class
     * @param type The consumer type to map
     * @return The class representing the consumer type
     */
    <T> Class<T> consumerTypeToClass(UserDefinedType<ConsumerApiDefinition> type);

    /**
     * Maps a provider type to the class that represents it.
     * 
     * @param <T>  The type of the class
     * @param type The provider type to map
     * @return The class representing the provider type
     */
    <T> Class<T> providerTypeToClass(UserDefinedType<ProviderApiDefinition> type);

}
