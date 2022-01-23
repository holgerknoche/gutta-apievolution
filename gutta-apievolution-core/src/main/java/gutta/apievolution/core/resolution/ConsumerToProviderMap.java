package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerUserDefinedType;
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

    void checkConsistency() {
        this.checkTypeAssociation(this.consumerToProviderType, this.consumerToProviderField);
    }

    private void checkTypeAssociation(Map<Type, Type> consumerToProviderType,
                                      Map<ConsumerField, ProviderField> consumerToProviderField) {
        ConsumerTypeConsistencyChecker checker = new ConsumerTypeConsistencyChecker();

        consumerToProviderType.forEach(checker::checkConsistency);
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

    private class ConsumerTypeConsistencyChecker implements TypeVisitor<Void> {

        private Type foreignType;

        private ConsumerToProviderTypeLookup typeLookup;

        public void checkConsistency(Type ownType, Type foreignType) {
            this.foreignType = foreignType;
            this.typeLookup = new ConsumerToProviderTypeLookup();
            ownType.accept(this);
        }

        @Override
        public Void handleEnumType(EnumType<?, ?, ?> enumType) {
            // No specific checks as of now
            return null;
        }

        @SuppressWarnings("unchecked")
        private <T extends Type> T resolveForeignType(Type ownType) {
            return (T) ConsumerToProviderMap.this.consumerToProviderType.get(ownType);
        }

        private ProviderField resolveForeignField(ConsumerField ownField) {
            return ConsumerToProviderMap.this.consumerToProviderField.get(ownField);
        }

        private Type determineMatchingProviderType(Type consumerType) {
            // TODO
            return null;
        }

        @Override
        public Void handleRecordType(RecordType<?, ?, ?> recordType) {
            // Assert that the types of the fields are compatible
            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                ConsumerField ownField = (ConsumerField) field;
                ProviderField foreignField = this.resolveForeignField(ownField);
                this.checkField(ownField, foreignField);
            }

            return null;
        }

        private void checkField(ConsumerField ownField, ProviderField foreignField) {
            // Make sure that the optionalities match
            Optionality consumerOptionality = ownField.getOptionality();
            Optionality providerOptionality = foreignField.getOptionality();

            if (consumerOptionality != providerOptionality) {
                throw new DefinitionResolutionException("Optionalities of " + ownField + " and " + foreignField +
                        " are not compatible.");
            }

            // Make sure that the types match
            Type ownType = ownField.getType();
            Type foreignType = foreignField.getType();

            // The foreign type must be matched against the provider image of the
            // current type
            Type expectedType = this.typeLookup.lookupType(ownType);

            if (!foreignType.equals(expectedType)) {
                throw new DefinitionResolutionException("Types of " + ownField + " (" + ownType + ") and of " +
                        foreignField + " (" + foreignType + ") do not match.");
            }
        }

    }

    /**
     * Type lookup for mapping consumer to provider types.
     */
    private class ConsumerToProviderTypeLookup extends TypeLookup<Type, Type> {

        @Override
        protected boolean isUserDefinedType(Type type) {
            return (type instanceof ConsumerUserDefinedType);
        }

        @Override
        protected Type mapUserDefinedType(Type type) {
            return consumerToProviderType.get(type);
        }
    }

}
