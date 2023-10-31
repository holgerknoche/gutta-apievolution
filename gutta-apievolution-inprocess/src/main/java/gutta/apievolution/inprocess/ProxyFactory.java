package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

public interface ProxyFactory {

    <T> T createProxy(Object providerApi, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap, Class<T> consumerApiType);

}
