package gutta.apievolution.inprocess.consumer.objectmapping;

public interface ConsumerApi {

    ConsumerResult testOperation(ConsumerParameter parameter);

    ConsumerResult operationWithMappedException(ConsumerParameter parameter) throws MappedConsumerException;

    ConsumerResult operationWithUnmappedException(ConsumerParameter parameter);

    ConsumerResult operationWithRuntimeException(ConsumerParameter parameter);
    
    ConsumerSuperType operationWithRepresentableSubtype(ConsumerParameter parameter);
    
    ConsumerSuperType operationWithUnrepresentableSubtype(ConsumerParameter parameter);
    
    ConsumerSuperType polyOperation(ConsumerSuperType parameter);

}
