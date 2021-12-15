package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ToMergedModelMap;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * This class represents a map from a consumer revision to a specific provider revision. It is complemented with
 * a mapping from the provider revision to the current provider's internal representation.
 */
class ConsumerToProviderMap {

    private final Map<Type, Type> consumerToProviderType;

    private final Map<ConsumerField, ProviderField> consumerToProviderField;

    private final Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember;

    public ConsumerToProviderMap(Map<Type, Type> consumerToProviderType,
                                 Map<ConsumerField, ProviderField> consumerToProviderField,
                                 Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember) {
        this.consumerToProviderType = consumerToProviderType;
        this.consumerToProviderField = consumerToProviderField;
        this.consumerToProviderMember = consumerToProviderMember;
    }

    /**
     * Composes this map with the given map to a provider's merged definition, resulting in a map from the consumer
     * definition of this map to the merged definition.
     * @param toMergedModelMap The map from the provider definition to the merged definition
     * @return A composed map of this and the given map
     */
    public ConsumerToProviderMap compose(ToMergedModelMap toMergedModelMap) {
        Map<Type, Type> composedTypeMap = composeMaps(this.consumerToProviderType,
                type -> toMergedModelMap.mapType(type).orElse(null));
        Map<ConsumerField, ProviderField> composedFieldMap = composeMaps(this.consumerToProviderField,
                field -> toMergedModelMap.mapField(field).orElse(null));
        Map<ConsumerEnumMember, ProviderEnumMember> composedMemberMap = composeMaps(this.consumerToProviderMember,
                member -> toMergedModelMap.mapEnumMember(member).orElse(null));

        return new ConsumerToProviderMap(composedTypeMap, composedFieldMap, composedMemberMap);
    }

    /**
     * Inverts this map to produce a map from the provider's definition to the consumer's definition.
     * @return see above
     */
    public ProviderToConsumerMap invert() {
        Map<Type, Type> invertedTypeMap = invertMap(this.consumerToProviderType, this::onAmbiguousType);
        Map<ProviderField, ConsumerField> invertedFieldMap = invertMap(this.consumerToProviderField,
                this::onAmbiguousField);
        Map<ProviderEnumMember, ConsumerEnumMember> invertedMemberMap = invertMap(this.consumerToProviderMember,
                this::onAmbiguousEnumMember);

        return new ProviderToConsumerMap(invertedTypeMap, invertedFieldMap, invertedMemberMap);
    }

    private void onAmbiguousType(Type type) {
        throw new DefinitionResolutionException("Ambiguous type " + type + ".");
    }

    private void onAmbiguousField(ProviderField field) {
        throw new DefinitionResolutionException("Ambiguous field " + field + ".");
    }

    private void onAmbiguousEnumMember(ProviderEnumMember enumMember) {
        throw new DefinitionResolutionException("Ambiguous enum member " + enumMember + ".");
    }

    private static <A, B, C> Map<A, C> composeMaps(Map<A, B> map1, Function<B, C> map2) {
        Map<A, C> composedMap = new HashMap<>(map1.size());

        for (Map.Entry<A, B> entry : map1.entrySet()) {
            C value = map2.apply(entry.getValue());

            if (value != null) {
                composedMap.put(entry.getKey(), value);
            }
        }

        return composedMap;
    }

    private static <A, B> Map<B, A> invertMap(Map<A, B> map, Consumer<B> onConflict) {
        Map<B, A> invertedMap = new HashMap<>(map.size());

        for (Map.Entry<A, B> entry : map.entrySet()) {
            A existingValue = invertedMap.put(entry.getValue(), entry.getKey());

            if (existingValue != null) {
                onConflict.accept(entry.getValue());
            }
        }

        return invertedMap;
    }

    Stream<Type> consumerTypes() {
        return this.consumerToProviderType.keySet().stream();
    }

    Type mapConsumerType(Type consumerType) {
        return this.consumerToProviderType.get(consumerType);
    }

}
