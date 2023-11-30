package gutta.apievolution.inprocess.consumer.objectmapping;

public interface ConsumerApi {

    ConsumerResult testOperation(ConsumerParameter parameter);
    
    ConsumerResult operationWithMappedException(ConsumerParameter parameter);
    
    ConsumerResult operationWithUnmappedException(ConsumerParameter parameter);
    
    ConsumerResult operationWithRuntimeException(ConsumerParameter parameter);
    
}
