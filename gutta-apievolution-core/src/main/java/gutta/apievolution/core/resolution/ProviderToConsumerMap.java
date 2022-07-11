package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.*;

/**
 * This class represents a map from a provider's internal representation to a
 * consumer revision.
 */
class ProviderToConsumerMap {

    private final Map<Type, Type> providerToConsumerType;

    private final Map<ProviderField, ConsumerField> providerToConsumerField;

    private final Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember;
    
    private final Map<ProviderOperation, ConsumerOperation> providerToConsumerOperation;

    public ProviderToConsumerMap(Map<Type, Type> providerToConsumerType,
            Map<ProviderField, ConsumerField> providerToConsumerField,
            Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember,
            Map<ProviderOperation, ConsumerOperation> providerToConsumerOperation) {
        this.providerToConsumerType = providerToConsumerType;
        this.providerToConsumerField = providerToConsumerField;
        this.providerToConsumerMember = providerToConsumerMember;
        this.providerToConsumerOperation = providerToConsumerOperation;
    }

    Collection<Type> providerTypes() {
        return Collections.unmodifiableSet(this.providerToConsumerType.keySet());
    }

    Collection<ProviderOperation> providerOperations() {
        return Collections.unmodifiableSet(this.providerToConsumerOperation.keySet());
    }
    
    Type mapProviderType(Type providerType) {
        return this.providerToConsumerType.get(providerType);
    }

    ConsumerField mapProviderField(ProviderField providerField) {
        return this.providerToConsumerField.get(providerField);
    }
    
    ConsumerEnumMember mapProviderEnumMember(ProviderEnumMember providerEnumMember) {
        return this.providerToConsumerMember.get(providerEnumMember);
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
                // When the current record has a supertype, ensure that it is mapped in a
                // compatible way
                RecordType<?, ?, ?> ownSuperType = recordType.getSuperType().get(); // NOSONAR IsPresent is called
                RecordType<?, ?, ?> foreignSuperType = foreignRecordType.getSuperType().orElseThrow(
                        () -> new DefinitionResolutionException("Missing supertype on " + foreignRecordType + "."));

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
                this.checkField(ownField, foreignField, foreignRecordType);
            }

            return null;
        }

        private void checkField(ProviderField ownField, ConsumerField foreignField, RecordType<?, ?, ?> foreignType) {
            Optionality ownOptionality = ownField.getOptionality();

            // Determine the usage by the consumer, since output-only types do not have to be mapped
            Usage usage = foreignType.getUsage();
            
            if (foreignField == null && ownOptionality == Optionality.MANDATORY && usage != Usage.OUTPUT) {
                // Report an error if a mandatory field is not mapped and is not used only for output
                throw new DefinitionResolutionException("Non-optional field " + ownField + " is not mapped.");
            }
        }

    }

}
