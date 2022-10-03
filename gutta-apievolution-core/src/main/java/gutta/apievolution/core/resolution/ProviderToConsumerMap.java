package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.ApiDefinitionMorphism;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.Usage;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerUserDefinedType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderUserDefinedType;
import gutta.apievolution.core.util.CheckResult;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

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

    protected CheckResult checkConsistency() {
        CheckResult superResult = super.checkConsistency();
        
        this.checkTypeAssociation();
        
        // TODO Join with own result
        return superResult;
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
