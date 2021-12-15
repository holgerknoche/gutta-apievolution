package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;

import java.util.stream.Stream;

/**
 * A definition resolution represents the result of the resolution of a client API definition against
 * a provider's revision history, or more precisely, against the internal representation resulting
 * from the revision history.
 */
public class DefinitionResolution {

    private final ConsumerToProviderMap consumerToProviderMap;

    private final ProviderToConsumerMap providerToConsumerMap;

    DefinitionResolution(ConsumerToProviderMap consumerToProviderMap,
                         ProviderToConsumerMap providerToConsumerMap) {
        this.consumerToProviderMap = consumerToProviderMap;
        this.providerToConsumerMap = providerToConsumerMap;
    }

    /**
     * Returns a stream of all mapped consumer types.
     * @return see above
     */
    public Stream<Type> consumerTypes() {
        return this.consumerToProviderMap.consumerTypes();
    }

    /**
     * Returns a stream of all mapped provider types.
     * @return see above
     */
    public Stream<Type> providerTypes() {
        return this.providerToConsumerMap.providerTypes();
    }

    /**
     * Maps a given consumer type to the corresponding provider type.
     * @param consumerType The consumer type to map
     * @return The provider type, if it exists
     */
    public Type mapConsumerType(Type consumerType) {
        return this.consumerToProviderMap.mapConsumerType(consumerType);
    }

    /**
     * Maps a given provider type to the corresponding consumer type.
     * @param providerType The provider type to map
     * @return The consumer type, if it exists
     */
    public Type mapProviderType(Type providerType) {
        return this.providerToConsumerMap.mapProviderType(providerType);
    }

}
