package gutta.apievolution.core.resolution;

import static gutta.apievolution.core.apimodel.Optionality.MANDATORY;
import static gutta.apievolution.core.apimodel.Optionality.OPTIONAL;
import static gutta.apievolution.core.util.MapUtil.composeMaps;
import static gutta.apievolution.core.util.MapUtil.invertMap;

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
import gutta.apievolution.core.apimodel.provider.ToMergedModelMap;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

/**
 * This class represents a map from a consumer revision to a specific provider
 * revision. It is complemented with a mapping from the provider revision to the
 * current provider's internal representation.
 */
class ConsumerToProviderMap extends ApiDefinitionMorphism<ConsumerApiDefinition, ProviderApiDefinition, 
    ConsumerUserDefinedType, ProviderUserDefinedType, ConsumerField, ProviderField, ConsumerEnumMember, 
    ProviderEnumMember, ConsumerOperation, ProviderOperation> {

    public ConsumerToProviderMap(Map<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType,
            Map<ConsumerField, ProviderField> consumerToProviderField,
            Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember,
            Map<ConsumerOperation, ProviderOperation> consumerToProviderOperation) {
        
        this(new TypeMap<>(consumerToProviderType), consumerToProviderField, 
                consumerToProviderMember, consumerToProviderOperation);
    }
    
    private ConsumerToProviderMap(TypeMap<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap,
            Map<ConsumerField, ProviderField> consumerToProviderField,
            Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember,
            Map<ConsumerOperation, ProviderOperation> consumerToProviderOperation) {
        super(typeMap, consumerToProviderField, consumerToProviderMember, consumerToProviderOperation);
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

        return new ConsumerToProviderMap(composedTypeMap, composedFieldMap, composedMemberMap, composedOperationMap);
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

        return new ProviderToConsumerMap(invertedTypeMap, invertedFieldMap, invertedMemberMap, invertedOperationMap);
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

    protected void checkConsistency() {
        super.checkConsistency();
        
        this.checkTypeAssociation(this.typeMap, this.fieldMap);
    }

    private void checkTypeAssociation(TypeMap<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType,
            Map<ConsumerField, ProviderField> consumerToProviderField) {
        ConsumerTypeConsistencyChecker checker = new ConsumerTypeConsistencyChecker();

        consumerToProviderType.forEach(checker::checkConsistency);
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

    private class ConsumerTypeConsistencyChecker implements TypeVisitor<Void> {

        private Type foreignType;

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
            return (T) ConsumerToProviderMap.this.typeMap.mapType(ownType);
        }

        private ProviderField resolveForeignField(ConsumerField ownField) {
            return ConsumerToProviderMap.this.fieldMap.get(ownField);
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
        
        private void checkField(ConsumerField ownField, ProviderField foreignField) {
            // Make sure that the optionalities are compatible
            boolean optionalityIsCompatible = this.isOptionalityCompatible(ownField, foreignField);
            if (!optionalityIsCompatible) {
                throw new DefinitionResolutionException(
                        "Optionalities of " + ownField + " and " + foreignField + " are not compatible.");
            }
            
            // Make sure that the types match
            Type ownType = ownField.getType();
            Type foreignType = foreignField.getType();

            // The foreign type must be matched against the provider image of the
            // current type
            Type expectedType = ConsumerToProviderMap.this.typeMap.mapType(ownType);

            if (!foreignType.equals(expectedType)) {
                throw new DefinitionResolutionException("Types of " + ownField + " (" + ownType + ") and of " +
                        foreignField + " (" + foreignType + ") do not match.");
            }
        }

    }

}
