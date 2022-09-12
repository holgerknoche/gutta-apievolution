package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.*;

/**
 * This class represents a map from a provider's internal representation to a
 * consumer revision.
 */
class ProviderToConsumerMap extends ApiDefinitionMorphism<ProviderApiDefinition, ConsumerApiDefinition,
    ProviderUserDefinedType, ConsumerUserDefinedType,
    ProviderField, ConsumerField,
    ProviderEnumMember, ConsumerEnumMember,
    ProviderOperation, ConsumerOperation> {

    public ProviderToConsumerMap(TypeMap<ProviderUserDefinedType, ConsumerUserDefinedType> providerToConsumerType,
            Map<ProviderField, ConsumerField> providerToConsumerField,
            Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember,
            Map<ProviderOperation, ConsumerOperation> providerToConsumerOperation) {
        
        super(providerToConsumerType, providerToConsumerField, providerToConsumerMember, providerToConsumerOperation);
    }

    Collection<Type> providerTypes() {
        return Collections.unmodifiableSet(this.typeMap.sourceTypes());
    }

    Collection<ProviderOperation> providerOperations() {
        return Collections.unmodifiableSet(this.operationMap.keySet());
    }
    
    Type mapProviderType(Type providerType) {
        return this.typeMap.mapType(providerType);
    }

    ConsumerField mapProviderField(ProviderField providerField) {
        return this.fieldMap.get(providerField);
    }
    
    ConsumerEnumMember mapProviderEnumMember(ProviderEnumMember providerEnumMember) {
        return this.memberMap.get(providerEnumMember);
    }

    protected void checkConsistency() {
        super.checkConsistency();
        
        this.checkTypeAssociation();
    }

    private void checkTypeAssociation() {
        ProviderTypeConsistencyChecker checker = new ProviderTypeConsistencyChecker();

        this.typeMap.forEach(checker::checkConsistency);
    }

    private class ProviderTypeConsistencyChecker implements TypeVisitor<Void> {

        private Type foreignType;

        public void checkConsistency(Type ownType, Type foreignType) {
            this.foreignType = foreignType;
            ownType.accept(this);
        }

        @SuppressWarnings("unchecked")
        private <T extends Type> T resolveForeignType(Type ownType) {
            return (T) ProviderToConsumerMap.this.typeMap.mapType(ownType);
        }

        private ConsumerField resolveForeignField(ProviderField ownField) {
            return ProviderToConsumerMap.this.fieldMap.get(ownField);
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
