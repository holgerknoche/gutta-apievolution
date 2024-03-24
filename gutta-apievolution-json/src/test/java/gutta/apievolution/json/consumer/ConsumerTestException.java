package gutta.apievolution.json.consumer;

public class ConsumerTestException extends RuntimeException {
    
    private final ConsumerTestExceptionData data;
    
    public ConsumerTestException(ConsumerTestExceptionData data) {
        this.data = data;
    }
    
    public Integer getExceptionField() {
        return this.data.getExceptionField();
    }

}
