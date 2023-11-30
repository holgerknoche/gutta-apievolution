package gutta.apievolution.inprocess;

/**
 * A {@link TypeMappingStrategy} encapsulates the strategy used for mapping consumer types to provider types and vice versa. Such strategies employ
 * {@link ValueMapper} objects which handle the technicalities of mapping the values of one type to another.
 */
public interface TypeMappingStrategy {

    /**
     * Returns the value mapper for the given type if it exists.
     * 
     * @param type The type for which the mapper is required
     * @return The appropriate value mapper or {@code null} if no such mapper exists, for instance, for unmapped types
     */
    ValueMapper mapperFor(Class<?> type);

}
