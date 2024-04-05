package gutta.apievolution.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.consumer.MappedExceptionData;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(7)
public class ConsumerTestException implements MappedExceptionData {
        
    private Integer exceptionField;
    
    public Integer getExceptionField() {
        return this.exceptionField;
    }
    
    public void setExceptionField(Integer exceptionField) {
        this.exceptionField = exceptionField;
    }
    
    @Override
    public RuntimeException createMappedException() {
        return new MappedConsumerTestException(this);
    }

}
