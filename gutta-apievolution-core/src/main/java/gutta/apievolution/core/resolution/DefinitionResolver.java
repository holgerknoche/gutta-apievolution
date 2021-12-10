package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

/**
 * The definition resolver resolves client API definitions against provider API revisions. The resulting resolution
 * allows the provider to transform data provided by the client into its own representation and vice-versa, thus
 * serving the client in a representation it is able to process.
 */
public class DefinitionResolver {

    /**
     * Resolves the given consumer API definition against a given provider revision history.
     * @param revisionMap The provider revision history, represented as a map from revision number to revision
     * @param supportedRevisions The set of supported revision numbers
     * @param consumerApi The consumer API to resolve
     */
    public void resolveConsumerDefinition(Map<Integer, ProviderApiDefinition> revisionMap,
                                          Set<Integer> supportedRevisions, ConsumerApiDefinition consumerApi) {

        Integer desiredRevision = consumerApi.getReferencedRevision();
        if (!supportedRevisions.contains(desiredRevision)) {
            throw new DefinitionResolutionException("Revision " + desiredRevision + " is not supported.");
        }

        ProviderApiDefinition providerApi = revisionMap.get(desiredRevision);
        if (providerApi == null) {
            throw new DefinitionResolutionException("Revision " + desiredRevision + " does not exists.");
        }

        this.resolveConsumerDefinitionAgainst(providerApi, consumerApi);
    }

    void resolveConsumerDefinitionAgainst(ProviderApiDefinition providerApi,
                                          ConsumerApiDefinition consumerApi) {
        // Step 1: Establish a type-type mapping for user-defined types
        Map<Type, Type> consumerToProviderType = this.createTypeMapping(providerApi, consumerApi);

        // Step 2: Establish mappings between members and fields for each user-defined type
        Map<ConsumerField, ProviderField> consumerToProviderField = this.createFieldMapping(consumerApi,
                consumerToProviderType);
        Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember = this.createMemberMapping(consumerApi,
                consumerToProviderType);

        // Step 3: Invert the maps to obtain the provider's view
        Map<Type, Type> providerToConsumerType = invertMap(consumerToProviderType, this::onTypeConflict);
        Map<ProviderField, ConsumerField> providerToConsumerField = invertMap(consumerToProviderField,
                this::onFieldConflict);
        Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember = invertMap(consumerToProviderMember,
                this::onEnumMemberConflict);

        // Step 4: Perform consistency checks on the maps
        this.checkConsumerMaps(consumerToProviderType, consumerToProviderField, consumerToProviderMember);
        this.checkProviderMaps(providerToConsumerType, providerToConsumerField, providerToConsumerMember);
    }

    private void onTypeConflict(Type providerType) {
        throw new DefinitionResolutionException("Ambiguous provider type " + providerType + ".");
    }

    private void onFieldConflict(ProviderField providerField) {
        throw new DefinitionResolutionException("Ambiguous provider field " + providerField + ".");
    }

    private void onEnumMemberConflict(ProviderEnumMember providerEnumMember) {
        throw new DefinitionResolutionException("Ambiguous provider enum member " + providerEnumMember + ".");
    }

    private static <K, V> Map<V, K> invertMap(Map<K, V> inMap, Consumer<V> onConflict) {
        Map<V, K> invertedMap = new HashMap<>(inMap.size());

        inMap.forEach((key, value) -> {
            K existingValue = invertedMap.put(value, key);
            if (existingValue != null) {
                onConflict.accept(value);
            }
        });

        return invertedMap;
    }

    private Map<Type, Type> createTypeMapping(ProviderApiDefinition providerApi, ConsumerApiDefinition consumerApi) {
        Map<Type, Type> consumerToProviderType = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerType : consumerApi.getUserDefinedTypes()) {
            String publicTypeName = consumerType.getPublicName();
            Type providerType = providerApi.resolveUserDefinedType(publicTypeName)
                    .orElseThrow(() -> new DefinitionResolutionException("No matching type for " + consumerType +
                            " (" + publicTypeName + ")."));
            this.assertMatchingType(consumerType, providerType);
            consumerToProviderType.put(consumerType, providerType);
        }

        return consumerToProviderType;
    }

    private void assertMatchingType(Type consumerType, Type providerType) {
        if (consumerType instanceof ConsumerEnumType && !(providerType instanceof ProviderEnumType)) {
            throw new DefinitionResolutionException("Consumer type '" + consumerType +
                    " is an enum, but provider type " + providerType + " is not.");
        }
        if (consumerType instanceof ConsumerRecordType && !(providerType instanceof ProviderRecordType)) {
            throw new DefinitionResolutionException("Consumer type '" + consumerType +
                    " is a record type, but provider type " + providerType + " is not.");
        }
    }

    private Map<ConsumerField, ProviderField> createFieldMapping(ConsumerApiDefinition consumerApi,
                                                                 Map<Type, Type> consumerToProviderType) {
        Map<ConsumerField, ProviderField> consumerToProviderField = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerUDT : consumerApi.getUserDefinedTypes()) {
            if (!(consumerUDT instanceof ConsumerRecordType)) {
                continue;
            }

            ConsumerRecordType consumerType = (ConsumerRecordType) consumerUDT;
            ProviderRecordType providerType = (ProviderRecordType) consumerToProviderType.get(consumerType);
            for (ConsumerField consumerField : consumerType.getDeclaredFields()) {
                String fieldName = consumerField.getPublicName();
                ProviderField providerField = providerType.resolveField(fieldName).orElseThrow(
                        () -> new DefinitionResolutionException("Missing field " + fieldName + " in provider type " +
                                providerType)
                );

                consumerToProviderField.put(consumerField, providerField);
            }
        }

        return consumerToProviderField;
    }

    private Map<ConsumerEnumMember, ProviderEnumMember> createMemberMapping(ConsumerApiDefinition consumerApi,
                                                                            Map<Type, Type> consumerToProviderType) {
        Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerUDT : consumerApi.getUserDefinedTypes()) {
            if (!(consumerUDT instanceof ConsumerEnumType)) {
                continue;
            }

            ConsumerEnumType consumerType = (ConsumerEnumType) consumerUDT;
            ProviderEnumType providerType = (ProviderEnumType) consumerToProviderType.get(consumerType);
            for (ConsumerEnumMember consumerMember : consumerType.getMembers()) {
                String memberName = consumerMember.getPublicName();
                ProviderEnumMember providerMember = providerType.resolveMember(memberName).orElseThrow(
                        () -> new DefinitionResolutionException("Missing member " + memberName + " in provider type " +
                                providerType)
                );

                consumerToProviderMember.put(consumerMember, providerMember);
            }
        }

        return consumerToProviderMember;
    }

    private void checkConsumerMaps(Map<Type, Type> consumerToProviderType,
                                   Map<ConsumerField, ProviderField> consumerToProviderField,
                                   Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember) {
        // Check the consumer-to-provider type associations for consistency
        this.checkConsumerToProviderTypeAssociation(consumerToProviderType, consumerToProviderField);
    }

    private void checkConsumerToProviderTypeAssociation(Map<Type, Type> consumerToProviderType,
                                                        Map<ConsumerField, ProviderField> consumerToProviderField) {
        ConsumerTypeConsistencyChecker checker = new ConsumerTypeConsistencyChecker(consumerToProviderType,
                consumerToProviderField);

        consumerToProviderType.forEach(
                (consumerType, providerType) -> checker.checkConsistency(consumerType, providerType)
        );
    }

    private void checkProviderMaps(Map<Type, Type> providerToConsumerType,
                                   Map<ProviderField, ConsumerField> providerToConsumerField,
                                   Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember) {
        // TODO
    }

    private class ConsumerTypeConsistencyChecker implements TypeVisitor<Void> {

        private final Map<Type, Type> consumerToProviderType;

        private final Map<ConsumerField, ProviderField> consumerToProviderField;

        private Type foreignType;

        public ConsumerTypeConsistencyChecker(Map<Type, Type> consumerToProviderType,
                                              Map<ConsumerField, ProviderField> consumerToProviderField) {
            this.consumerToProviderType = consumerToProviderType;
            this.consumerToProviderField = consumerToProviderField;
        }

        public void checkConsistency(Type ownType, Type foreignType) {
            this.foreignType = foreignType;
            ownType.accept(this);
        }

        @Override
        public Void handleEnumType(EnumType<?, ?, ?> enumType) {
            // No specific checks as of now

            return null;
        }

        @SuppressWarnings("unchecked")
        private <T extends Type> T resolveForeignType(Type ownType) {
            return (T) this.consumerToProviderType.get(ownType);
        }

        private ProviderField resolveForeignField(ConsumerField ownField) {
            return this.consumerToProviderField.get(ownField);
        }

        @Override
        public Void handleRecordType(RecordType<?, ?, ?> recordType) {
            RecordType<?, ?, ?> foreignRecordType = (RecordType<?, ?, ?>) this.foreignType;

            if (recordType.getSuperType().isPresent()) {
                // When the current record has a supertype, ensure that it is mapped in a compatible way
                RecordType<?, ?, ?> ownSuperType = recordType.getSuperType().get();
                RecordType<?, ?, ?> foreignSuperType = foreignRecordType.getSuperType().orElseThrow(
                        () -> new DefinitionResolutionException("Missing supertype on " + foreignRecordType + ".")
                );

                Type mappedForeignSupertype = this.resolveForeignType(ownSuperType);
                if (!foreignSuperType.equals(mappedForeignSupertype)) {
                    throw new DefinitionResolutionException("Supertype of " + foreignRecordType + " is mapped to " +
                            mappedForeignSupertype + ", expected " + foreignSuperType + ".");
                }
            }

            // Assert that the types of the fields are compatible
            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                ConsumerField ownField = (ConsumerField) field;
                ProviderField foreignField = this.resolveForeignField(ownField);
                this.checkField(ownField, foreignField);
            }

            return null;
        }

        private void checkField(ConsumerField ownField, ProviderField foreignField) {
            // TODO
            Optionality consumerOptionality = ownField.getOptionality();
            Optionality providerOptionality = foreignField.getOptionality();

        }

    }

}
