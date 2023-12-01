package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractProxyApiMappingStrategy;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.TypeMappingStrategy;

import java.lang.reflect.InvocationHandler;

// Comments on dynamic proxies (esp. vs. explicit object conversion)
// - Proxy creation is not free, though may be cheaper if few attributes are actually read
// - Interface requirement is cumbersome for exceptions
// - equals() is either identity or inherited from the implementation. In the latter case,
//   an object that is indistinguishable from the consumer perspective may not be equal due
//   to fields only visible to the provider

/**
 * The dynamic proxy API mapping strategy employs lazily created dynamic proxies for adapting provider types to the consumer's view.
 */
public class DynamicProxyApiMappingStrategy extends AbstractProxyApiMappingStrategy {

    @Override
    protected InvocationHandler createApiInvocationHandler(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition,
            DefinitionResolution definitionResolution, TypeClassMap typeClassMap) {

        MethodMappingStrategy methodMappingStrategy = new DynamicProxyMethodMappingStrategy(consumerApiDefinition, definitionResolution, typeClassMap);
        TypeMappingStrategy typeMappingStrategy = new DynamicProxyTypeMappingStrategy(consumerApiDefinition, definitionResolution, typeClassMap);

        return new DynamicProxyInvocationHandler(providerApiObject, methodMappingStrategy, typeMappingStrategy);
    }

}
