package gutta.apievolution.inprocess;

import static java.lang.reflect.Proxy.newProxyInstance;

import java.lang.reflect.InvocationHandler;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

/**
 * Abstract supertype for API mapping strategies that rely on dynamic proxies for mapping invocations of API methods.
 */
public abstract class AbstractProxyApiMappingStrategy implements ApiMappingStrategy {

    @Override
    @SuppressWarnings("unchecked")
    public <T> T mapApi(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap, Class<T> consumerApiType) {
        
                
        TypeClassMap typeClassMap = new TypeClassMap(typeToClassMap, consumerApiDefinition, definitionResolution);
        InvocationHandler invocationHandler = this.createApiInvocationHandler(providerApiObject, consumerApiDefinition,
                definitionResolution, typeClassMap);

        Class<?>[] implementedInterfaces = new Class<?>[] { consumerApiType };
        return (T) newProxyInstance(this.getClass().getClassLoader(), implementedInterfaces, invocationHandler);
    }

    protected abstract InvocationHandler createApiInvocationHandler(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition,
            DefinitionResolution definitionResolution, TypeClassMap typeClassMap);   

}
