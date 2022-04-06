package gutta.apievolution.core.apimodel;

/**
 * Visitor interface for visiting the types of an API model.
 *
 * @param <T> The result type of the visitor's operation
 */
public interface TypeVisitor<T> {

    /**
     * Handles an atomic type.
     *
     * @param atomicType The atomic type to handle
     * @return The visitor operation's result
     */
    default T handleAtomicType(AtomicType atomicType) {
        return null;
    }

    /**
     * Handles a bounded list type.
     *
     * @param boundedListType The bounded list type to handle
     * @return The visitor operation's result
     */
    default T handleBoundedListType(BoundedListType boundedListType) {
        return null;
    }

    /**
     * Handles a bounded string type.
     *
     * @param boundedStringType The bounded string type to handle
     * @return The visitor operation's result
     */
    default T handleBoundedStringType(BoundedStringType boundedStringType) {
        return null;
    }

    /**
     * Handles an enumeration type.
     *
     * @param enumType The enumeration type to handle
     * @return The visitor operation's result
     */
    default T handleEnumType(EnumType<?, ?, ?> enumType) {
        return null;
    }

    /**
     * Handles a numeric type.
     *
     * @param numericType The numeric type to handle
     * @return The visitor operation's result
     */
    default T handleNumericType(NumericType numericType) {
        return null;
    }

    /**
     * Handles a record type.
     *
     * @param recordType The record type to handle
     * @return The visitor operation's result
     */
    default T handleRecordType(RecordType<?, ?, ?> recordType) {
        return null;
    }

    /**
     * Handles an unbounded list type.
     *
     * @param unboundedListType The unbounded list type to handle
     * @return The visitor operation's result
     */
    default T handleUnboundedListType(UnboundedListType unboundedListType) {
        return null;
    }

    /**
     * Handles an unbounded string type.
     *
     * @param unboundedStringType The unbounded string type to handle
     * @return The visitor operation's result
     */
    default T handleUnboundedStringType(UnboundedStringType unboundedStringType) {
        return null;
    }

}
