package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
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

    ConsumerField mapProviderField(ProviderField providerField) {
        return this.providerToConsumerField.get(providerField);
    }

    void checkConsistency() {
        this.checkTypeAssociation();
    }

    private void checkTypeAssociation() {
        ProviderTypeConsistencyChecker checker = new ProviderTypeConsistencyChecker();

        providerToConsumerType.forEach(checker::checkConsistency);
    }

    private class ProviderTypeConsistencyChecker implements TypeVisitor<Void> {

        private Type foreignType;

        public void checkConsistency(Type ownType, Type foreignType) {
            this.foreignType = foreignType;
            ownType.accept(this);
        }

        @SuppressWarnings("unchecked")
        private <T extends Type> T resolveForeignType(Type ownType) {
            return (T) ProviderToConsumerMap.this.providerToConsumerType.get(ownType);
        }

        private ConsumerField resolveForeignField(ProviderField ownField) {
            return ProviderToConsumerMap.this.providerToConsumerField.get(ownField);
        }

        @Override
        public Void handleEnumType(EnumType<?, ?, ?> enumType) {
            // No specific checks as of now
            return null;
        }

        @Override
        public Void handleRecordType(RecordType<?, ?, ?> recordType) {
            RecordType<?, ?, ?> foreignRecordType = (RecordType<?, ?, ?>) this.foreignType;

            if (recordType.getSuperType().isPresent()) {
                // When the current record has a supertype, ensure that it is mapped in a compatible way
                RecordType<?, ?, ?> ownSuperType = recordType.getSuperType().get(); // NOSONAR IsPresent is called
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
            for (Field<?, ?> field : recordType.getDeclaredFields()) { // NOSONAR Type is correct
                ProviderField ownField = (ProviderField) field;
                ConsumerField foreignField = this.resolveForeignField(ownField);
                this.checkField(ownField, foreignField);
            }

            return null;
        }

        private void checkField(ProviderField ownField, ConsumerField foreignField) {
            Optionality ownOptionality = ownField.getOptionality();

            if (foreignField == null) {
                if (ownOptionality == Optionality.MANDATORY) {
                    // Report an error if a mandatory field is not mapped
                    throw new DefinitionResolutionException("Non-optional field " + ownField + " is not mapped.");
                } else {
                    return;
                }
            }

            Optionality foreignOptionality = foreignField.getOptionality();
            if (ownOptionality.isMorePermissiveThan(foreignOptionality)) {
                throw new DefinitionResolutionException("Consumer optionality on field " + foreignField +
                        " is too restrictive.");
            }
        }

    }

}
