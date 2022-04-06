package gutta.apievolution.core.apimodel.provider;

/**
 * Visitor interface for {@link ProviderApiDefinitionElement}.
 *
 * @param <T> The return type for the operation encoded in this visitor
 */
public interface ProviderApiDefinitionElementVisitor<T> {

    /**
     * Performs the visitor operation for an enum member.
     *
     * @param enumMember The enum member to operate on
     * @return The result of the operation
     */
    default T handleProviderEnumMember(ProviderEnumMember enumMember) {
        return null;
    }

    /**
     * Performs the visitor operation for an enum type.
     *
     * @param enumType The enum type to operate on
     * @return The result of the operation
     */
    default T handleProviderEnumType(ProviderEnumType enumType) {
        return null;
    }

    /**
     * Performs the visitor operation for a field.
     *
     * @param field The field to operate on
     * @return The result of the operation
     */
    default T handleProviderField(ProviderField field) {
        return null;
    }

    /**
     * Performs the visitor operation for a record type.
     *
     * @param recordType The record type to operate on
     * @return The result of the operation
     */
    default T handleProviderRecordType(ProviderRecordType recordType) {
        return null;
    }

    /**
     * Performs the visitor operation for an operation.
     *
     * @param operation The operation to operate on
     * @return The result of the operation
     */
    default T handleProviderOperation(ProviderOperation operation) {
        return null;
    }

}
