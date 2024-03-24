package gutta.apievolution.json.provider;

public abstract class MappableException extends RuntimeException {
    
    private static final long serialVersionUID = 7813270991898861920L;
    
    private final Object exceptionData;
    
    protected MappableException(Object exceptionData) {
        this.exceptionData = exceptionData;
    }
    
    public Object getExceptionData() {
        return this.exceptionData;
    }

}
