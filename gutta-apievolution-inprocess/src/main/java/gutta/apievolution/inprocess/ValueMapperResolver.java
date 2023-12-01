package gutta.apievolution.inprocess;

/**
 * A {@link ValueMapperResolver} is responsible for resolving {@link ValueMapper value mappers} for types.
 */
public interface ValueMapperResolver {

    /**
     * Resolves the value mapper that is able to map values of the given type.
     * 
     * @param type The type whose values are to be mapped
     * @return The appropriate value mapper or {@code null} if no such mapper exists
     */
    ValueMapper resolveValueMapperFor(Class<?> type);

}
