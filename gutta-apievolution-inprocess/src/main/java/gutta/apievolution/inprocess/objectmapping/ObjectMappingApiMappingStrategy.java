package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractProxyApiMappingStrategy;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.TypeMappingStrategy;

import java.lang.reflect.InvocationHandler;

/**
 * This mapping strategy creates actual consumer and provider objects, mapping values as appropriate.
 */
public class ObjectMappingApiMappingStrategy extends AbstractProxyApiMappingStrategy {

    @Override
    protected InvocationHandler createApiInvocationHandler(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition,
            DefinitionResolution definitionResolution, TypeClassMap typeClassMap) {

        MethodMappingStrategy methodMappingStrategy = new ObjectMappingMethodMappingStrategy(consumerApiDefinition, definitionResolution, typeClassMap);
        TypeMappingStrategy typeMappingStrategy = new ObjectMappingTypeMappingStrategy(consumerApiDefinition, definitionResolution, typeClassMap);

        return new ObjectMappingInvocationHandler(providerApiObject, methodMappingStrategy, typeMappingStrategy);
    }

}
