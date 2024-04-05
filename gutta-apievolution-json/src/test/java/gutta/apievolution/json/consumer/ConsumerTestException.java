package gutta.apievolution.json.consumer;

public class ConsumerTestException extends RuntimeException {
    
    private static final long serialVersionUID = -7450054012675597283L;
    
    private final ConsumerTestExceptionData data;
    
    public ConsumerTestException(ConsumerTestExceptionData data) {
        this.data = data;
    }
    
    public Integer getExceptionField() {
        return this.data.getExceptionField();
    }

}
