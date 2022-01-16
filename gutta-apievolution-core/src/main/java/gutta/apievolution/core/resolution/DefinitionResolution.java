package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderField;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * A definition resolution represents the result of the resolution of a client API definition against
 * a provider's revision history, or more precisely, against the internal representation resulting
 * from the revision history.
 */
public class DefinitionResolution {

    private final ConsumerToProviderMap consumerToProviderMap;

    private final ProviderToConsumerMap providerToConsumerMap;

    private final Map<String, Type> consumerTypeMap;

    private final Map<String, Type> providerTypeMap;

    DefinitionResolution(ConsumerToProviderMap consumerToProviderMap,
                         ProviderToConsumerMap providerToConsumerMap) {
        this.consumerToProviderMap = consumerToProviderMap;
        this.providerToConsumerMap = providerToConsumerMap;

        this.consumerTypeMap = createTypeMap(consumerToProviderMap.consumerTypes());
        this.providerTypeMap = createTypeMap(providerToConsumerMap.providerTypes());
    }

    private static Map<String, Type> createTypeMap(Stream<Type> types) {
        return types.filter(UserDefinedType.class::isInstance)
                .map(type -> (UserDefinedType<?>) type)
                .collect(Collectors.toMap(UserDefinedType::getInternalName, Function.identity()));
    }

    /**
     * Returns a stream of all mapped consumer types.
     * @return see above
     */
    public Stream<Type> consumerTypes() {
        return this.consumerToProviderMap.consumerTypes();
    }

    /**
     * Resolves the given internal name into a consumer type.
     * @param internalName The internal name of the desired type
     * @return The consumer type, if it exists
     */
    public Type resolveConsumerType(String internalName) {
        return this.consumerTypeMap.get(internalName);
    }

    /**
     * Returns a stream of all mapped provider types.
     * @return see above
     */
    public Stream<Type> providerTypes() {
        return this.providerToConsumerMap.providerTypes();
    }

    /**
     * Resolves the given internal name into a provider type.
     * @param internalName The internal name of the desired type
     * @return The consumer type, if it exists
     */
    public Type resolveProviderType(String internalName) {
        return this.providerTypeMap.get(internalName);
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

    /**
     * Maps a given provider field to the corresponding consumer field.
     * @param providerField The provider field to map
     * @return The corresponding consumer field, if it exists
     */
    public ConsumerField mapProviderField(ProviderField providerField) {
        return this.providerToConsumerMap.mapProviderField(providerField);
    }

}
