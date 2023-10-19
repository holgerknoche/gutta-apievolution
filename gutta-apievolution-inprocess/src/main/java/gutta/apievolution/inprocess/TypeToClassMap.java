package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;

public interface TypeToClassMap {

    <T> Class<T> consumerRecordTypeToClass(ConsumerRecordType type);
    
    <T> Class<T> providerRecordTypeToClass(ProviderRecordType type);
    
}
