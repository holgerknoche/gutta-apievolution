package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.ApiDefinitionMorphism;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.apimodel.Usage;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.consumer.ConsumerUserDefinedType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderUserDefinedType;
import gutta.apievolution.core.validation.ValidationResult;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * This class represents a map from a provider's internal representation to a
 * consumer revision.
 */
class ProviderToConsumerMap extends ApiDefinitionMorphism<ProviderApiDefinition, ConsumerApiDefinition,
    ProviderUserDefinedType, ConsumerUserDefinedType,
    ProviderField, ConsumerField,
    ProviderEnumMember, ConsumerEnumMember,
    ProviderOperation, ConsumerOperation> {

    public ProviderToConsumerMap(ProviderApiDefinition sourceDefinition, ConsumerApiDefinition targetDefinition,
            TypeMap<ProviderUserDefinedType, ConsumerUserDefinedType> providerToConsumerType,
            Map<ProviderField, ConsumerField> providerToConsumerField,
            Map<ProviderEnumMember, ConsumerEnumMember> providerToConsumerMember,
            Map<ProviderOperation, ConsumerOperation> providerToConsumerOperation) {
        
        super(sourceDefinition, targetDefinition, providerToConsumerType, providerToConsumerField,
                providerToConsumerMember, providerToConsumerOperation);
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

    protected ValidationResult checkConsistency() {
        ValidationResult superResult = super.checkConsistency();
        
        ValidationResult ownResult = new ValidationResult();
        this.checkForUnmappedTypes(ownResult);
        
        Set<UserDefinedType<ProviderApiDefinition>> reachableTypes = this.determineReachableTypes();
        this.checkForUnmappedFields(ownResult, reachableTypes);
        
        return superResult.joinWith(ownResult);
    }
    
    private Set<UserDefinedType<ProviderApiDefinition>> determineReachableTypes() {
        return determineReachableTypes(this.providerOperations());
    }
    
    private void checkForUnmappedTypes(ValidationResult result) {
        // The API morphisms themselves already guarantee fundamental consistency of the type mapping, so we only have
        // to check for warnings
        
        // All exception types of an operation should be mapped, but do not have to be
        for (ProviderOperation operation : this.providerOperations()) {
            for (ProviderRecordType providerExceptionType : operation.getThrownExceptions()) {
                ConsumerRecordType consumerExceptionType = (ConsumerRecordType) this.mapProviderType(providerExceptionType);
                
                if (consumerExceptionType == null) {
                    result.addWarningMessage("Unmapped exception type '" + providerExceptionType + "' on operation '" + operation + "'.");
                }
            }
        }
    }
    
    private void checkForUnmappedFields(ValidationResult result, Set<UserDefinedType<ProviderApiDefinition>> reachableTypes) {
        for (UserDefinedType<?> udt : reachableTypes) {
            if (udt instanceof ProviderRecordType) {
                ProviderRecordType providerType = (ProviderRecordType) udt;
                ConsumerRecordType consumerType = (ConsumerRecordType) this.mapUserDefinedType(providerType).orElse(null);
                
                if (consumerType == null) {
                    // No further checks for unmapped types
                    continue;
                }
                
                for (ProviderField field : providerType) {
                    this.ensureMandatoryFieldIsMapped(field, consumerType, result);
                }
            }
        }
    }
    
    private void ensureMandatoryFieldIsMapped(ProviderField providerField, ConsumerRecordType consumerType,
            ValidationResult result) {
        ConsumerField consumerField = this.mapProviderField(providerField);
        Optionality providerOptionality = providerField.getOptionality();

        // Determine the usage by the consumer, since output-only types do not have to be mapped
        Usage usage = consumerType.getUsage();
        
        if (consumerField == null && providerOptionality == Optionality.MANDATORY && usage != Usage.OUTPUT) {
            // Report an error if a mandatory field is not mapped and is not used only for output
            result.addErrorMessage("Mandatory field " + providerField + " is not mapped.");
        }
    }

}
