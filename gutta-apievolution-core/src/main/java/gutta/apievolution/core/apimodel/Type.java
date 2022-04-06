package gutta.apievolution.core.apimodel;

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

}
