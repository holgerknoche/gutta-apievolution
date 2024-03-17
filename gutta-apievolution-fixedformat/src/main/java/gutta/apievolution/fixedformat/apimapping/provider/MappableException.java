package gutta.apievolution.fixedformat.apimapping.provider;

public class MappableException extends RuntimeException {
    
    private static final long serialVersionUID = -1043936476421116168L;
    
    private final Object exceptionData;
    
    public MappableException(Object exceptionData) {
        this.exceptionData = exceptionData;
    }
    
    public Object getExceptionData() {
        return this.exceptionData;
    }

}
