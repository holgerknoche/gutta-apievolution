package gutta.apievolution.core.apimodel;

import java.util.Collection;

/**
 * Generic interface for all types, such as basic types or user-defined types.
 */
public interface Type {

    /**
     * Accepts the given type visitor.
     *
     * @param <R>     The visitor operation's return type
     * @param visitor The visitor to accept
     * @return The visitor operation's result
     */
    <R> R accept(TypeVisitor<R> visitor);
    
    /**
     * Denotes whether this type is unbounded.
     * 
     * @return {@code True} if this type is unbounded, otherwise {@code false}.
     */
    default boolean isUnbounded() {
        return false;
    }
    
    /**
     * Denotes whether this type is user-defined.
     * 
     * @return {@code True} if this is a user-defined type, otherwise {@code false}.
     */
    default boolean isUserDefined() {
        return false;
    }

    /**
     * Returns the most specific (with respect to inheritance) type of a collection
     * of types.
     * 
     * @param <T>   The type of the types in the collection
     * @param types The types to determine the most specific type of
     * @return The most specific type
     * @throws InvalidApiDefinitionException If no (unique) specific type can be
     *                                       determined
     */
    static <T extends Type> T mostSpecificTypeOf(Collection<T> types) {
        if (types.isEmpty()) {
            throw new IllegalArgumentException("Unable to determine the most specific type of an empty collection.");
        }
        
        T mostSpecificType = null;
        
        for (T type : types) {
            if (mostSpecificType == null) {
                // The first type encountered is always the most specific one up to this point
                mostSpecificType = type;
                continue;
            } else if (type instanceof RecordType && mostSpecificType instanceof RecordType) {
                // If both the most specific type and the current type are records, they may be in a inheritance hierarchy
                // and the current type may be more specific
                
                RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;
                RecordType<?, ?, ?> mostSpecificRecordType = (RecordType<?, ?, ?>) mostSpecificType;
                
                if (mostSpecificRecordType.isSupertypeOf(recordType)) {
                    // The current type is more specific than the most specific one up to this point
                    mostSpecificType = type;
                } else if (!recordType.isSupertypeOf(mostSpecificRecordType)) {
                    // The current type is a supertype of the most specific one, which is fine
                } else {
                    // The current type and the most specific type are not in the same inheritance hierarchy
                    throw new IllegalArgumentException("Types '" + type + "' and '" + mostSpecificType + "' are not in the same inheritance hierarchy.");
                }                
            } else {
                // If not both the current type and the most specific type are records, they cannot be in an inheritance hierarchy and
                // must therfore be the same
                if (!type.equals(mostSpecificType)) {
                    throw new IllegalArgumentException("Types '" + type + "' and '" + mostSpecificType + "' are not in the same inheritance hierarchy.");
                }
            }
        }
        
        return mostSpecificType;
    }
    
}
