package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.ApiDefinitionMorphism;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.Usage;
import gutta.apievolution.core.apimodel.UserDefinedType;
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
import gutta.apievolution.core.apimodel.provider.ToMergedModelMap;
import gutta.apievolution.core.util.CheckResult;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static gutta.apievolution.core.apimodel.Optionality.MANDATORY;
import static gutta.apievolution.core.apimodel.Optionality.OPTIONAL;
import static gutta.apievolution.core.util.MapUtil.composeMaps;
import static gutta.apievolution.core.util.MapUtil.invertMap;

/**
 * This class represents a map from a consumer revision to a specific provider
 * revision. It is complemented with a mapping from the provider revision to the
 * current provider's internal representation.
 */
class ConsumerToProviderMap extends ApiDefinitionMorphism<ConsumerApiDefinition, ProviderApiDefinition, 
    ConsumerUserDefinedType, ProviderUserDefinedType, ConsumerField, ProviderField, ConsumerEnumMember, 
    ProviderEnumMember, ConsumerOperation, ProviderOperation> {

    public ConsumerToProviderMap(ConsumerApiDefinition sourceDefinition, ProviderApiDefinition targetDefinition,
            Map<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType,
            Map<ConsumerField, ProviderField> consumerToProviderField,
            Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember,
            Map<ConsumerOperation, ProviderOperation> consumerToProviderOperation) {
        
        this(sourceDefinition, targetDefinition, new TypeMap<>(consumerToProviderType), consumerToProviderField, 
                consumerToProviderMember, consumerToProviderOperation);
    }
    
    private ConsumerToProviderMap(ConsumerApiDefinition sourceDefinition, ProviderApiDefinition targetDefinition,
            TypeMap<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap,
            Map<ConsumerField, ProviderField> consumerToProviderField,
            Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember,
            Map<ConsumerOperation, ProviderOperation> consumerToProviderOperation) {
        super(sourceDefinition, targetDefinition, typeMap, consumerToProviderField, consumerToProviderMember,
                consumerToProviderOperation);
    }
    
    /**
     * Composes this map with the given map to a provider's merged definition,
     * resulting in a map from the consumer definition of this map to the merged
     * definition.
     *
     * @param toMergedModelMap The map from the provider definition to the merged
     *                         definition
     * @return A composed map of this and the given map
     */
    public ConsumerToProviderMap compose(ToMergedModelMap toMergedModelMap) {
        TypeMap<ConsumerUserDefinedType, ProviderUserDefinedType> composedTypeMap =
                this.typeMap.compose(type -> toMergedModelMap.mapUserDefinedType(type).orElse(null));                
        Map<ConsumerField, ProviderField> composedFieldMap = composeMaps(this.fieldMap,
                field -> toMergedModelMap.mapField(field).orElse(null));
        Map<ConsumerEnumMember, ProviderEnumMember> composedMemberMap = composeMaps(this.memberMap,
                member -> toMergedModelMap.mapEnumMember(member).orElse(null));
        Map<ConsumerOperation, ProviderOperation> composedOperationMap = composeMaps(this.operationMap,
                operation -> toMergedModelMap.mapOperation(operation).orElse(null));

        return new ConsumerToProviderMap(this.sourceDefinition, toMergedModelMap.getTargetDefinition(), 
                composedTypeMap, composedFieldMap, composedMemberMap, composedOperationMap);
    }

    /**
     * Inverts this map to produce a map from the provider's definition to the
     * consumer's definition.
     *
     * @return see above
     */
    public ProviderToConsumerMap invert() {
        TypeMap<ProviderUserDefinedType, ConsumerUserDefinedType> invertedTypeMap = 
                this.typeMap.invert(this::onAmbiguousType);
        Map<ProviderField, ConsumerField> invertedFieldMap = invertMap(this.fieldMap,
                this::onAmbiguousField);
        Map<ProviderEnumMember, ConsumerEnumMember> invertedMemberMap = invertMap(this.memberMap,
                this::onAmbiguousEnumMember);
        Map<ProviderOperation, ConsumerOperation> invertedOperationMap = invertMap(this.operationMap,
                this::onAmbiguousOperation);

        return new ProviderToConsumerMap(this.targetDefinition, this.sourceDefinition, invertedTypeMap,
                invertedFieldMap, invertedMemberMap, invertedOperationMap);
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
    
    private void onAmbiguousOperation(ProviderOperation operation) {
        throw new DefinitionResolutionException("Ambiguous operation " + operation + ".");
    }

    protected CheckResult checkConsistency() {
        CheckResult superResult = super.checkConsistency();
        
        CheckResult ownResult = new CheckResult();
        // Make sure that all elements are mapped
        this.checkAllElementsAreMapped(ownResult);
        // Make sure that supertypes are mapped consistently
        this.checkSuperTypeConsistency(ownResult);
        // Check optionalities of fields
        this.checkFieldOptionalities(ownResult);
                
        // Join with own result
        return superResult.joinWith(ownResult);
    }
    
    private void checkAllElementsAreMapped(CheckResult result) {
        MemberMapChecker memberMapChecker = new MemberMapChecker(result);
        
        // Check that all UDTs as well as their elements are mapped
        List<UserDefinedType<ConsumerApiDefinition>> udts = this.sourceDefinition.getUserDefinedTypes();
        for (UserDefinedType<ConsumerApiDefinition> udt : udts) {
            if (!this.mapUserDefinedType((ConsumerUserDefinedType) udt).isPresent()) {
                result.addErrorMessage("User-defined type '" + udt + "' is not mapped.");
            }
            
            memberMapChecker.checkUDT(udt);
        }
        
        // Check that all operations are mapped
        for (ConsumerOperation operation : this.sourceDefinition.getOperations()) {
            if (!this.mapOperation(operation).isPresent()) {
                result.addErrorMessage("Operation '" + operation + "' is not mapped."); 
            }
        }
    }
        
    private void checkFieldOptionalities(CheckResult result) {
        this.fieldMap.forEach(
                (sourceField, targetField) -> this.ensureCompatibleOptionality(sourceField, targetField, result)
        );
    }

    private void ensureCompatibleOptionality(ConsumerField sourceField, ProviderField targetField, CheckResult result) {
        if (!this.isOptionalityCompatible(sourceField, targetField)) {
            result.addErrorMessage("Optionalities of " + sourceField + " and " + targetField + " are not compatible.");
        }
    }
        
    private boolean isOptionalityCompatible(ConsumerField ownField, ProviderField foreignField) {
        Optionality consumerOptionality = ownField.getOptionality();
        Optionality providerOptionality = foreignField.getOptionality();
        
        // The consumer usage is the one that counts
        Usage usage = ownField.getOwner().getUsage();
        
        switch (usage) {
        case INPUT:
            // For types only used as input, the consumer can be more strict than the
            // provider. Furthermore, opt-in and optional are equivalent.
            if (providerOptionality == MANDATORY) {
                return (consumerOptionality == MANDATORY);
            } else {
                return true;
            }
            
        case OUTPUT:
            // For types only used as output, the consumer can be more permissive than
            // the provider. Furthermore, opt-in and mandatory are equivalent.
            if (providerOptionality == OPTIONAL) {
                return (consumerOptionality == OPTIONAL);
            } else {
                return true;
            }
            
        case IN_OUT:
            // If the type is used for both input and output, we have a mixture of the previous cases
            if (providerOptionality == MANDATORY) {
                // The type is used as input and therefore, mandatory fields must be provided
                return (consumerOptionality == MANDATORY);
            } else if (providerOptionality == OPTIONAL) {
                // The type is used as output and therefore, optional fields cannot be expected
                return (consumerOptionality == OPTIONAL);
            } else {
                // Opt-in on the provider side is compatible with all optionalities on the consumer side
                return true;
            }
            
        case NONE:
            // If the type is not used at all, anything goes
            return true;
            
        default:
            throw new IllegalArgumentException("Unsupported usage " + usage + ".");
        }
    }
    
    Collection<Type> consumerTypes() {
        return Collections.unmodifiableSet(this.typeMap.sourceTypes());
    }

    Collection<ConsumerOperation> consumerOperations() {
        return Collections.unmodifiableSet(this.operationMap.keySet());
    }

    Type mapConsumerType(Type consumerType) {
        return this.typeMap.mapType(consumerType);
    }

    ProviderField mapConsumerField(ConsumerField consumerField) {
        return this.fieldMap.get(consumerField);
    }

    ProviderEnumMember mapConsumerMember(ConsumerEnumMember consumerEnumMember) {
        return this.memberMap.get(consumerEnumMember);
    }

    ProviderOperation mapConsumerOperation(ConsumerOperation consumerOperation) {
        return this.operationMap.get(consumerOperation);
    }
    
    private class MemberMapChecker implements TypeVisitor<Void> {
        
        private final CheckResult result;
        
        public MemberMapChecker(CheckResult result) {
            this.result = result;
        }
        
        public void checkUDT(Type type) {
            type.accept(this);
        }
        
        @Override
        public Void handleEnumType(EnumType<?, ?, ?> enumType) {
            for (EnumMember<?, ?> member : enumType) {
                if (mapConsumerMember((ConsumerEnumMember) member) == null) {
                    this.result.addErrorMessage("Enum member '" + member + "' is not mapped.");
                }
            }
            
            return null;
        }
        
        @Override
        public Void handleRecordType(RecordType<?, ?, ?> recordType) {
            for (Field<?, ?> field : recordType) {
                if (!mapField((ConsumerField) field).isPresent()) {
                    this.result.addErrorMessage("Field '" +  field + "' is not mapped.");
                }
            }
            
            return null;
        }
        
    }

}
