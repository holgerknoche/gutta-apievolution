package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.*;
import java.util.function.Function;
import java.util.stream.*;

/**
 * A definition resolution represents the result of the resolution of a client
 * API definition against a provider's revision history, or more precisely,
 * against the internal representation resulting from the revision history.
 */
public class DefinitionResolution {

    private final ConsumerToProviderMap consumerToProviderMap;

    private final ProviderToConsumerMap providerToConsumerMap;

    private final Map<String, Type> consumerTypeMap;

    private final Map<String, Type> providerTypeMap;

    DefinitionResolution(ConsumerToProviderMap consumerToProviderMap, ProviderToConsumerMap providerToConsumerMap) {
        this.consumerToProviderMap = consumerToProviderMap;
        this.providerToConsumerMap = providerToConsumerMap;

        this.consumerTypeMap = createTypeMap(consumerToProviderMap.consumerTypes());
        this.providerTypeMap = createTypeMap(providerToConsumerMap.providerTypes());
    }

    private static Map<String, Type> createTypeMap(Collection<Type> types) {
        return types.stream().filter(UserDefinedType.class::isInstance).map(type -> (UserDefinedType<?>) type)
                .collect(Collectors.toMap(UserDefinedType::getInternalName, Function.identity()));
    }

    /**
     * Returns a collection of all mapped consumer types.
     *
     * @return see above
     */
    public Collection<Type> consumerTypes() {
        return this.consumerToProviderMap.consumerTypes();
    }

    /**
     * Returns a collection of all mapped consumer operations.
     *
     * @return see above
     */
    public Collection<ConsumerOperation> consumerOperations() {
        return this.consumerToProviderMap.consumerOperations();
    }

    /**
     * Resolves the given internal name into a consumer type.
     *
     * @param internalName The internal name of the desired type
     * @return The consumer type, if it exists
     */
    public Type resolveConsumerType(String internalName) {
        return this.consumerTypeMap.get(internalName);
    }

    /**
     * Returns a collection of all mapped provider types.
     *
     * @return see above
     */
    public Collection<Type> providerTypes() {
        return this.providerToConsumerMap.providerTypes();
    }
    
    /**
     * Returns a collection of all mapped provider operations.
     *
     * @return see above
     */
    public Collection<ProviderOperation> providerOperations() {
        return this.providerToConsumerMap.providerOperations();
    }

    /**
     * Resolves the given internal name into a provider type.
     *
     * @param internalName The internal name of the desired type
     * @return The consumer type, if it exists
     */
    public Type resolveProviderType(String internalName) {
        return this.providerTypeMap.get(internalName);
    }

    /**
     * Maps a given consumer type to the corresponding provider type.
     *
     * @param consumerType The consumer type to map
     * @return The provider type, if it exists
     */
    public Type mapConsumerType(Type consumerType) {
        return this.consumerToProviderMap.mapConsumerType(consumerType);
    }

    /**
     * Maps a given provider type to the corresponding consumer type.
     *
     * @param providerType The provider type to map
     * @return The consumer type, if it exists
     */
    public Type mapProviderType(Type providerType) {
        return this.providerToConsumerMap.mapProviderType(providerType);
    }

    /**
     * Maps a given provider field to the corresponding consumer field.
     *
     * @param providerField The provider field to map
     * @return The corresponding consumer field, if it exists
     */
    public ConsumerField mapProviderField(ProviderField providerField) {
        return this.providerToConsumerMap.mapProviderField(providerField);
    }
    
    /**
     * Maps a given provider enum member to the corresponding consumer enum member.
     *
     * @param providerEnumMember The provider enum member to map
     * @return The corresponding consumer enum member, if it exists
     */
    public ConsumerEnumMember mapProviderEnumMember(ProviderEnumMember providerEnumMember) {
        return this.providerToConsumerMap.mapProviderEnumMember(providerEnumMember);
    }

    /**
     * Maps a given consumer field to the corresponding provider field.
     *
     * @param consumerField The provider field to map
     * @return The corresponding provider field, if it exists
     */
    public ProviderField mapConsumerField(ConsumerField consumerField) {
        return this.consumerToProviderMap.mapConsumerField(consumerField);
    }

    /**
     * Maps a given consumer enum member to the corresponding provider enum member.
     *
     * @param consumerEnumMember The provider enum member to map
     * @return The corresponding provider enum member, if it exists
     */
    public ProviderEnumMember mapConsumerEnumMember(ConsumerEnumMember consumerEnumMember) {
        return this.consumerToProviderMap.mapConsumerMember(consumerEnumMember);
    }

    /**
     * Maps a given consumer operation to the corresponding provider operation.
     *
     * @param consumerOperation The provider operation to map
     * @return The corresponding operation, if it exists
     */
    public ProviderOperation mapConsumerOperation(ConsumerOperation consumerOperation) {
        return this.consumerToProviderMap.mapConsumerOperation(consumerOperation);
    }

}
