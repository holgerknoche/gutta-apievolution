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

    protected CheckResult checkConsistency() {
        CheckResult superResult = super.checkConsistency();
        
        CheckResult ownResult = new CheckResult();
        this.ensureMandatoryFieldsAreMapped(ownResult);
        
        return superResult.joinWith(ownResult);
    }
    
    private void ensureMandatoryFieldsAreMapped(CheckResult result) {
        for (UserDefinedType<?> udt : this.sourceDefinition.getUserDefinedTypes()) {
            if (udt instanceof ProviderRecordType) {
                ProviderRecordType providerType = (ProviderRecordType) udt;
                ConsumerRecordType consumerType = (ConsumerRecordType) this.mapUserDefinedType(providerType).orElse(null);
                
                for (ProviderField field : providerType) {
                    this.ensureMandatoryFieldIsMapped(field, consumerType, result);
                }
            }
        }
    }
    
    private void ensureMandatoryFieldIsMapped(ProviderField providerField, ConsumerRecordType consumerType,
            CheckResult result) {
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
