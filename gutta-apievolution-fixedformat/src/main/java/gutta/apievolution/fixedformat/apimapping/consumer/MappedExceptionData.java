package gutta.apievolution.fixedformat.apimapping.consumer;

/**
 * Interface for mapped exception data, which can be transformed into an actual exception.
 */
public interface MappedExceptionData {
    
    /**
     * Creates the actual exception for the exception data.
     * 
     * @return see above
     */
    RuntimeException createMappedException();

}
