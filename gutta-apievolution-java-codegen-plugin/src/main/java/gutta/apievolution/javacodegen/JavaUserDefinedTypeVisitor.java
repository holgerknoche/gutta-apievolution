package gutta.apievolution.javacodegen;

/**
 * Visitor interface for Java UDTs.
 * 
 * @param <T> The result type of the visitor operation
 */
public interface JavaUserDefinedTypeVisitor<T> {

    /**
     * Performs the operation for a Java enum.
     * 
     * @param javaEnum The enum to process
     * @return The result of the operation
     */
    default T handleJavaEnum(JavaEnum javaEnum) {
        return null;
    }

    /**
     * Performs the operation for a Java exception.
     * 
     * @param javaException The exception to process
     * @return The result of the operation
     */
    default T handleJavaException(JavaException javaException) {
        return null;
    }

    /**
     * Performs the operation for a Java interface.
     * 
     * @param javaInterface The interface to process
     * @return The result of the operation
     */
    default T handleJavaInterface(JavaInterface javaInterface) {
        return null;
    }

}
