package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.CheckResult;

import java.util.Map;
import java.util.Optional;

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
        
        this.checkFieldTypes(result);
        this.checkOperationTypes(result);
        
        return result;
    }
    
    private void checkFieldTypes(CheckResult result) {
        // Ensure that field types are mapped consistently by this morphism
        this.fieldMap.forEach((sourceField, targetField) -> this.ensureConsistentFieldTypeMapping(sourceField,
                targetField, result));
    }
    
    private void ensureConsistentFieldTypeMapping(Field<?, ?> sourceField, Field<?, ?> targetField,
            CheckResult result) {
        // Make sure that this(type(f)) = type(this(f)) for all mapped fields
        Type sourceType = sourceField.getType();
        Type mappedSourceType = this.mapType(sourceType).orElse(null);
        Type targetType = targetField.getType();
        
        if (mappedSourceType == null) {
            result.addErrorMessage("Type '" + sourceType + "' of mapped field '" + sourceField + "' is not mapped.");
        } else if (!mappedSourceType.equals(targetType)) {
            result.addErrorMessage("Type '" + sourceType + "' of mapped field '" + sourceField + 
                    "' is mapped to incompatible type '" + mappedSourceType + "' instead of '" + targetType + "'.");
        }
    }
    
    private void checkOperationTypes(CheckResult result) {
        // Ensure that operation parameter and result types are mapped consistently by this morphism
        this.operationMap.forEach(
                (sourceOperation, targetOperation) -> this.ensureConsistentOperationTypeMapping(sourceOperation, targetOperation, result)
                );
    }

    private void ensureConsistentOperationTypeMapping(Operation<?, ?, ?> sourceOperation, Operation<?, ?, ?> targetOperation,
            CheckResult result) {
        // TODO
    }
    
}
