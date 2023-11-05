package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.MappingStrategy;
import gutta.apievolution.inprocess.UDTToClassMap;

public class ObjectMappingStrategy implements MappingStrategy {

    @Override
    public <T> T createProxy(Object providerApi, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap, Class<T> consumerApiType) {
        // TODO Auto-generated method stub
        return null;
    }

}
