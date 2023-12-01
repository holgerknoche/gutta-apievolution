package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

/**
 * An {@link ApiMappingStrategy} represents a strategy to map a consumer API to a provider API, represented by an implementation object.
 */
public interface ApiMappingStrategy {

    /**
     * Creates a consumer API object to adapt a given provider API object to the expectations of the consumer.
     * 
     * @param <T>                   The type of the consumer API
     * @param providerApiObject     The provider API object to use
     * @param consumerApiDefinition The definition of the consumer API
     * @param definitionResolution  The resolution of the consumer API against the provider API
     * @param typeToClassMap        A mapping of the API types to the classes representing them
     * @param consumerApiType       The type of the consumer API
     * @return A consumer API object that can be used to invoke operations on the provider API
     */
    <T> T mapApi(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, UDTToClassMap typeToClassMap,
            Class<T> consumerApiType);

}
