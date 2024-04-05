package gutta.apievolution.json.consumer;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("ConsumerTestException")
public class ConsumerTestExceptionData implements MappedExceptionData {
    
    private int exceptionField;
    
    public int getExceptionField() {
        return this.exceptionField;
    }
    
    public void setExceptionField(int exceptionField) {
        this.exceptionField = exceptionField;
    }

    @Override
    public RuntimeException createMappedException() {
        return new ConsumerTestException(this);
    }

}
