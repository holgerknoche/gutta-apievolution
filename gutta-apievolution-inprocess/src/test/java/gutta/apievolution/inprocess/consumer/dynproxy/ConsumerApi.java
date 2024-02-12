package gutta.apievolution.inprocess.consumer.dynproxy;

public interface ConsumerApi {

    ConsumerResult testOperation(ConsumerParameter parameter);

    ConsumerResult operationWithMappedException(ConsumerParameter parameter);

    ConsumerResult operationWithUnmappedException(ConsumerParameter parameter);

    ConsumerResult operationWithRuntimeException(ConsumerParameter parameter);
    
    ConsumerSuperType operationWithRepresentableSubtype(ConsumerParameter parameter);
    
    ConsumerSuperType operationWithUnrepresentableSubtype(ConsumerParameter parameter);
    
    ConsumerSuperType polyOperation(ConsumerSuperType parameter);

}
