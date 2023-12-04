package gutta.apievolution.inprocess;

/**
 * A {@link ValueMapper} specifies the mapping of a specific type.
 */
public interface ValueMapper {

    /**
     * Returns whether the given value is representable in the domain of this mapper.
     * 
     * @param value The value in question
     * @return {@code True}, if the value is representable, otherwise {@code false}
     */
    default boolean isRepresentable(Object value) {
        return true;
    }
    
    /**
     * Maps the given value according to the rules of this mapper.
     * 
     * @param value The value to map
     * @return The mapped value
     */
    Object mapValue(Object value);

}
