package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ToMergedModelMap;

import java.util.Map;

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
        // TODO
        return null;
    }

}
