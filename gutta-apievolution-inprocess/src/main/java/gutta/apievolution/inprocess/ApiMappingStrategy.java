package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

public interface ApiMappingStrategy {

    <T> T mapApi(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap, Class<T> consumerApiType);

}
