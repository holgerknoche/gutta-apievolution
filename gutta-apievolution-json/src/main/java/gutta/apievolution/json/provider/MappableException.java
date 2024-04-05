package gutta.apievolution.json.provider;

/**
 * Special exception type to denote an exception that is mappable and transferrable to a consumer.
 */
public abstract class MappableException extends RuntimeException {
    
    private static final long serialVersionUID = 7813270991898861920L;
    
    private final Object exceptionData;
   
    /**
     * Creates a new exception with the given data object. 
     * 
     * @param exceptionData The object that contains the data for this exception
     */
    protected MappableException(Object exceptionData) {
        this.exceptionData = exceptionData;
    }
    
    /**
     * Returns the data object contained in this exception.
     * 
     * @return see above
     */
    public Object getExceptionData() {
        return this.exceptionData;
    }

}
