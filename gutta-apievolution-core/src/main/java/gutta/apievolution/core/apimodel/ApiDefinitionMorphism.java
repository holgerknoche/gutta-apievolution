package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.CheckResult;

import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public abstract class ApiDefinitionMorphism<A1 extends ApiDefinition<A1, ?>, A2 extends ApiDefinition<A2, ?>, 
    T1 extends UserDefinedType<A1>, T2 extends UserDefinedType<A2>, 
    F1 extends Field<?, F1>, F2 extends Field<?, F2>, M1 extends EnumMember<?, M1>,
    M2 extends EnumMember<?, M2>, O1 extends Operation<?, O1, ?>, O2 extends Operation<?, O2, ?>> {
    
    protected final TypeMap<T1, T2> typeMap;
    
    protected final Map<F1, F2> fieldMap;

    protected final Map<M1, M2> memberMap;

    protected final Map<O1, O2> operationMap;

    protected ApiDefinitionMorphism(TypeMap<T1, T2> typeMap, Map<F1, F2> fieldMap, Map<M1, M2> memberMap, Map<O1, O2> operationMap) {
        this.typeMap = typeMap;
        this.fieldMap = fieldMap;
        this.memberMap = memberMap;
        this.operationMap = operationMap;
    }
    
    public <T extends Type> Optional<T> mapType(Type sourceType) {
        return Optional.ofNullable(this.typeMap.mapType(sourceType));
    }
    
    public Optional<T2> mapUserDefinedType(T1 sourceType) {
        return Optional.ofNullable(this.typeMap.mapUserDefinedType(sourceType));
    }
    
    public Optional<F2> mapField(F1 sourceField) {
        return Optional.ofNullable(this.fieldMap.get(sourceField));
    }
    
    public Optional<M2> mapEnumMember(M1 sourceMember) {
        return Optional.ofNullable(this.memberMap.get(sourceMember));
    }
    
    public Optional<O2> mapOperation(O1 sourceOperation) {
        return Optional.ofNullable(this.operationMap.get(sourceOperation));
    }
    
    protected CheckResult checkConsistency() {
        CheckResult result = new CheckResult();
        
        this.checkFieldMapping(result);
        this.checkEnumMemberMapping(result);
        this.checkOperationMapping(result);
        
        return result;
    }
    
    private <E> void ensureConsistentTypeMapping(E sourceElement, E targetElement, Function<E, Type> operation, 
            CheckResult result, BiFunction<E, Type, String> onUnmappedType,
            ConsistencyCheckErrorMessageProvider<E> onIncompatibleType) {
        
        Type sourceType = operation.apply(sourceElement);
        Type mappedSourceType = this.mapType(sourceType).orElse(null);
        Type targetType = operation.apply(targetElement);
        
        if (mappedSourceType == null) {
            result.addErrorMessage(onUnmappedType.apply(sourceElement, sourceType));
        } else if (!mappedSourceType.equals(targetType)) {
            result.addErrorMessage(onIncompatibleType.createMessage(sourceElement, sourceType, mappedSourceType, targetType));
        }
    }
    
    private void checkFieldMapping(CheckResult result) {
        // Ensure that field types are mapped consistently by this morphism
        this.fieldMap.forEach((sourceField, targetField) -> this.ensureConsistentFieldTypeMapping(sourceField,
                targetField, result));
    }
        
    private void ensureConsistentFieldTypeMapping(Field<?, ?> sourceField, Field<?, ?> targetField,
            CheckResult result) {
        // Make sure that the record type containing the member is mapped in a compatible way
        this.ensureConsistentTypeMapping(sourceField, targetField, Field::getOwner, result,
                (source, sourceType) -> "Record type '" + sourceType + "' containing field '" + source + "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Record type '" + sourceType + "' containing field '" + source + 
                "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType + "'.");
        
        // Make sure that the field type is mapped in a compatible way
        this.ensureConsistentTypeMapping(sourceField, targetField, Field::getType, result,
                (source, sourceType) -> "Type '" + sourceType + "' of mapped field '" + source + "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Type '" + sourceType + "' of mapped field '" + source + 
                "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType + "'.");        
    }
    
    private void checkEnumMemberMapping(CheckResult result) {
        this.memberMap.forEach(
                (sourceMember, targetMember) -> this.ensureConsistentMemberTypeMapping(sourceMember, targetMember, result)
                );
    }
    
    private void ensureConsistentMemberTypeMapping(EnumMember<?, ?> sourceMember, EnumMember<?, ?> targetMember,
            CheckResult result) {
        // Make sure that the enum type containing the member is mapped in a compatible way
        this.ensureConsistentTypeMapping(sourceMember, targetMember, EnumMember::getOwner, result,
                (source, sourceType) -> "Enum type '" + sourceType + "' containing member '" + source + "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Enum type '" + sourceType + "' containing member '" + source + 
                "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType + "'.");
    }
    
    private void checkOperationMapping(CheckResult result) {
        // Ensure that operation parameter and result types are mapped consistently by this morphism
        this.operationMap.forEach(
                (sourceOperation, targetOperation) -> this.ensureConsistentOperationTypeMapping(sourceOperation, targetOperation, result)
                );
    }

    private void ensureConsistentOperationTypeMapping(Operation<?, ?, ?> sourceOperation, Operation<?, ?, ?> targetOperation,
            CheckResult result) {
        // Make sure that operation parameter and result types are mapped in a compatible way
        this.ensureConsistentTypeMapping(sourceOperation, targetOperation, Operation::getParameterType, result,
                (source, sourceType) -> "Parameter type '" + sourceType + "' of mapped operation '" + source + "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Parameter type '" + sourceType + "' of mapped operation '" + source + 
                "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType + "'.");
        
        this.ensureConsistentTypeMapping(sourceOperation, targetOperation, Operation::getReturnType, result,
                (source, sourceType) -> "Return type '" + sourceType + "' of mapped operation '" + source + "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Return type '" + sourceType + "' of mapped operation '" + source + 
                "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType + "'.");
    }

    private interface ConsistencyCheckErrorMessageProvider<E> {
        
        String createMessage(E sourceElement, Type sourceType, Type mappedType, Type expectedMappedType);
        
    }
    
}
