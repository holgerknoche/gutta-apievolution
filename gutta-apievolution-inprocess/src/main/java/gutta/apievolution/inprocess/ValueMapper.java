package gutta.apievolution.inprocess;

/**
 * A {@link ValueMapper} specifies the mapping of a specific type.
 */
public interface ValueMapper {

    /**
     * Maps the given value according to the rules of this mapper.
     * 
     * @param value The value to map
     * @return The mapped value
     */
    Object mapValue(Object value);

}
