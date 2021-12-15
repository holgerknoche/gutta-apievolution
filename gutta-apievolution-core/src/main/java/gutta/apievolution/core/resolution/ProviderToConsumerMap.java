package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;

import java.util.Map;
import java.util.stream.Stream;

/**
 * This class represents a map from a provider's internal representation to a consumer revision.
 */
class ProviderToConsumerMap {

    private final Map<Type, Type> providerToConsumerType;

    private final Map<ProviderField, ConsumerField> providerToConsumerField;

    private final Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember;

    public ProviderToConsumerMap(Map<Type, Type> providerToConsumerType,
                                 Map<ProviderField, ConsumerField> providerToConsumerField,
                                 Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember) {
        this.providerToConsumerType = providerToConsumerType;
        this.providerToConsumerField = providerToConsumerField;
        this.providerToConsumerMember = providerToConsumerMember;
    }

    Stream<Type> providerTypes() {
        return this.providerToConsumerType.keySet().stream();
    }

    Type mapProviderType(Type providerType) {
        return this.providerToConsumerType.get(providerType);
    }

}
