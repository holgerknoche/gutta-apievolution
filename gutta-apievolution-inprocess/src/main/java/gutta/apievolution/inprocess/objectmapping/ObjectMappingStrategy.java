package gutta.apievolution.inprocess.objectmapping;

import java.lang.reflect.InvocationHandler;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractProxyApiMappingStrategy;
import gutta.apievolution.inprocess.TypeClassMap;

public class ObjectMappingStrategy extends AbstractProxyApiMappingStrategy {

    @Override
    protected InvocationHandler createApiInvocationHandler(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition,
            DefinitionResolution definitionResolution, TypeClassMap typeClassMap) {
        // TODO Auto-generated method stub
        return null;
    }

}
