package gutta.apievolution.core.apimodel.consumer;

/**
 * Visitor interface for {@link ConsumerApiDefinitionElement}.
 * @param <T> The return type for the operation encoded in this visitor
 */
public interface ConsumerApiDefinitionElementVisitor<T> {

    /**
     * Performs the visitor operation for an enum member.
     * @param enumMember The enum member to operate on
     * @return The result of the operation
     */
    default T handleConsumerEnumMember(ConsumerEnumMember enumMember) {
        return null;
    }

    /**
     * Performs the visitor operation for an enum type.
     * @param enumType The enum type to operate on
     * @return The result of the operation
     */
    default T handleConsumerEnumType(ConsumerEnumType enumType) {
        return null;
    }

    /**
     * Performs the visitor operation for a field.
     * @param field The field to operate on
     * @return The result of the operation
     */
    default T handleConsumerField(ConsumerField field) {
        return null;
    }

    /**
     * Performs the visitor operation for a record type.
     * @param recordType The record type to operate on
     * @return The result of the operation
     */
    default T handleConsumerRecordType(ConsumerRecordType recordType) {
        return null;
    }

    /**
     * Performs the visitor operation for a service.
     * @param service The service to operate on
     * @return The result of the operation
     */
    default T handleConsumerService(ConsumerService service) {
        return null;
    }

    /**
     * Performs the visitor operation for a service operation.
     * @param serviceOperation The service operation to operate on
     * @return The result of the operation
     */
    default T handleConsumerServiceOperation(ConsumerServiceOperation serviceOperation) {
        return null;
    }

}
