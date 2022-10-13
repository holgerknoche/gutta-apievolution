package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.CheckResult;

import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * This class represents a morphism between two API definitions.
 *
 * @param <A1> The type of the first API definition
 * @param <A2> The type of the second API definition
 * @param <T1> The type of user-defined types from the first API definition
 * @param <T2> The type of user-defined types from the second API definition
 * @param <F1> The type of field from the first API definition
 * @param <F2> The type of field from the second API definition
 * @param <M1> The type of enum member from the first API definition
 * @param <M2> The type of enum member from the second API definition
 * @param <O1> The type of operation from the first API definition
 * @param <O2> The type of operation from the second API definition
 */
public abstract class ApiDefinitionMorphism<A1 extends ApiDefinition<A1, ?>, A2 extends ApiDefinition<A2, ?>,
    T1 extends UserDefinedType<A1>, T2 extends UserDefinedType<A2>,
    F1 extends Field<?, F1>, F2 extends Field<?, F2>,
    M1 extends EnumMember<?, M1>, M2 extends EnumMember<?, M2>,
    O1 extends Operation<?, O1, ?>, O2 extends Operation<?, O2, ?>> {

    protected final A1 sourceDefinition;
    
    protected final A2 targetDefinition;
    
    protected final TypeMap<T1, T2> typeMap;

    protected final Map<F1, F2> fieldMap;

    protected final Map<M1, M2> memberMap;

    protected final Map<O1, O2> operationMap;

    /**
     * Creates a new morphism from the given data.
     * @param sourceDefinition The source definition of the morphism
     * @param targetDefinition The target definition of the morphism
     * @param typeMap The type map on user-defined maps to use
     * @param fieldMap The field map to use
     * @param memberMap The enum member map to use
     * @param operationMap The operation map to use
     */
    protected ApiDefinitionMorphism(A1 sourceDefinition, A2 targetDefinition, TypeMap<T1, T2> typeMap, Map<F1, F2> fieldMap,
            Map<M1, M2> memberMap, Map<O1, O2> operationMap) {
        this.sourceDefinition = sourceDefinition;
        this.targetDefinition = targetDefinition;
        this.typeMap = typeMap;
        this.fieldMap = fieldMap;
        this.memberMap = memberMap;
        this.operationMap = operationMap;
    }

    /**
     * Returns the source definition of this morphism.
     * @return see above
     */
    public A1 getSourceDefinition() {
        return sourceDefinition;
    }
    
    /**
     * Returns the target definition of this morphism.
     * @return see above
     */
    public A2 getTargetDefinition() {
        return targetDefinition;
    }
    
    /**
     * Maps the given type.
     * @param <T> The expected type of type
     * @param sourceType The type to map
     * @return The mapped type, if it exists
     */
    public <T extends Type> Optional<T> mapType(Type sourceType) {
        return Optional.ofNullable(this.typeMap.mapType(sourceType));
    }

    /**
     * Explicitly maps a user-defined type.
     * @param sourceType The type to map
     * @return The mapped type, if it exists
     */
    public Optional<T2> mapUserDefinedType(T1 sourceType) {
        return Optional.ofNullable(this.typeMap.mapUserDefinedType(sourceType));
    }

    /**
     * Maps the given field.
     * @param sourceField The field to map
     * @return The mapped field, if it exists
     */
    public Optional<F2> mapField(F1 sourceField) {
        return Optional.ofNullable(this.fieldMap.get(sourceField));
    }

    /**
     * Maps the given enum member.
     * @param sourceMember The enum member to map
     * @return The mapped member, if it exists
     */
    public Optional<M2> mapEnumMember(M1 sourceMember) {
        return Optional.ofNullable(this.memberMap.get(sourceMember));
    }

    /**
     * Maps the given operation.
     * @param sourceOperation The operation to map
     * @return The mapped operation, if it exists
     */
    public Optional<O2> mapOperation(O1 sourceOperation) {
        return Optional.ofNullable(this.operationMap.get(sourceOperation));
    }

    /**
     * Checks this morphism for consistency.
     * @return The result of the check
     */
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
            result.addErrorMessage(
                    onIncompatibleType.createMessage(sourceElement, sourceType, mappedSourceType, targetType));
        }
    }

    private void checkFieldMapping(CheckResult result) {
        // Ensure that field types are mapped consistently by this morphism
        this.fieldMap.forEach(
                (sourceField, targetField) -> this.ensureConsistentFieldTypeMapping(sourceField, targetField, result));
    }

    private void ensureConsistentFieldTypeMapping(Field<?, ?> sourceField, Field<?, ?> targetField,
            CheckResult result) {
        // Make sure that the record type containing the member is mapped in a
        // compatible way
        this.ensureConsistentTypeMapping(sourceField, targetField, Field::getOwner, result,
                (source, sourceType) -> "Record type '" + sourceType + "' containing field '" + source +
                        "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Record type '" + sourceType +
                        "' containing field '" + source + "' is mapped to incompatible type '" + mappedType +
                        "' instead of '" + expectedType + "'.");

        // Make sure that the field type is mapped in a compatible way
        this.ensureConsistentTypeMapping(sourceField, targetField, Field::getType, result,
                (source, sourceType) -> "Type '" + sourceType + "' of mapped field '" + source + "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Type '" + sourceType + "' of mapped field '" +
                        source + "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType +
                        "'.");
    }

    private void checkEnumMemberMapping(CheckResult result) {
        this.memberMap.forEach((sourceMember, targetMember) -> this.ensureConsistentMemberTypeMapping(sourceMember,
                targetMember, result));
    }

    private void ensureConsistentMemberTypeMapping(EnumMember<?, ?> sourceMember, EnumMember<?, ?> targetMember,
            CheckResult result) {
        // Make sure that the enum type containing the member is mapped in a compatible
        // way
        this.ensureConsistentTypeMapping(sourceMember, targetMember, EnumMember::getOwner, result,
                (source, sourceType) -> "Enum type '" + sourceType + "' containing member '" + source +
                        "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Enum type '" + sourceType + "' containing member '" +
                        source + "' is mapped to incompatible type '" + mappedType + "' instead of '" + expectedType +
                        "'.");
    }

    private void checkOperationMapping(CheckResult result) {
        // Ensure that operation parameter and result types are mapped consistently by
        // this morphism
        this.operationMap.forEach((sourceOperation, targetOperation) -> this
                .ensureConsistentOperationTypeMapping(sourceOperation, targetOperation, result));
    }

    private void ensureConsistentOperationTypeMapping(Operation<?, ?, ?> sourceOperation,
            Operation<?, ?, ?> targetOperation, CheckResult result) {
        // Make sure that operation parameter and result types are mapped in a
        // compatible way
        this.ensureConsistentTypeMapping(sourceOperation, targetOperation, Operation::getParameterType, result,
                (source, sourceType) -> "Parameter type '" + sourceType + "' of mapped operation '" + source +
                        "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Parameter type '" + sourceType +
                        "' of mapped operation '" + source + "' is mapped to incompatible type '" + mappedType +
                        "' instead of '" + expectedType + "'.");

        this.ensureConsistentTypeMapping(sourceOperation, targetOperation, Operation::getReturnType, result,
                (source, sourceType) -> "Return type '" + sourceType + "' of mapped operation '" + source +
                        "' is not mapped.",
                (source, sourceType, mappedType, expectedType) -> "Return type '" + sourceType +
                        "' of mapped operation '" + source + "' is mapped to incompatible type '" + mappedType +
                        "' instead of '" + expectedType + "'.");
    }

    private interface ConsistencyCheckErrorMessageProvider<E> {

        String createMessage(E sourceElement, Type sourceType, Type mappedType, Type expectedMappedType);

    }

}
