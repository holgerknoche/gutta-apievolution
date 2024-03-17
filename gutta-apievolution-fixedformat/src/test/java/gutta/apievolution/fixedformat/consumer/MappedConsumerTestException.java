package gutta.apievolution.fixedformat.consumer;

public class MappedConsumerTestException extends RuntimeException {
    
    private static final long serialVersionUID = 8471901201367707599L;
    
    private final ConsumerTestException exceptionData;
    
    public MappedConsumerTestException(ConsumerTestException exceptionData) {
        this.exceptionData = exceptionData;
    }
    
    public Integer getExceptionField() {
        return this.exceptionData.getExceptionField();
    }

}
