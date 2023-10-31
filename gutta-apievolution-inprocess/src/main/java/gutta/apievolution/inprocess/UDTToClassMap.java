package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;

public interface UDTToClassMap {

    <T> Class<T> consumerTypeToClass(UserDefinedType<ConsumerApiDefinition> type);
    
    <T> Class<T> providerTypeToClass(UserDefinedType<ProviderApiDefinition> type);
    
}
