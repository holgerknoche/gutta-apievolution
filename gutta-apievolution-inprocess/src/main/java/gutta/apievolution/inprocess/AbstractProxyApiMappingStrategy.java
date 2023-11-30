package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.lang.reflect.InvocationHandler;

import static java.lang.reflect.Proxy.newProxyInstance;

/**
 * Abstract supertype for API mapping strategies that rely on dynamic proxies for mapping invocations of API methods.
 */
public abstract class AbstractProxyApiMappingStrategy implements ApiMappingStrategy {

    @Override
    @SuppressWarnings("unchecked")
    public <T> T mapApi(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap, Class<T> consumerApiType) {

        TypeClassMap typeClassMap = new TypeClassMap(typeToClassMap, consumerApiDefinition, definitionResolution);
        InvocationHandler invocationHandler = this.createApiInvocationHandler(providerApiObject, consumerApiDefinition, definitionResolution, typeClassMap);

        Class<?>[] implementedInterfaces = new Class<?>[] { consumerApiType };
        return (T) newProxyInstance(this.getClass().getClassLoader(), implementedInterfaces, invocationHandler);
    }

    /**
     * Creates a concrete invocation handler using the given data.
     * 
     * @param providerApiObject     The provider API object to invoke the methods on
     * @param consumerApiDefinition The API definition used by the consumer
     * @param definitionResolution  The resolution of the consumer API definition against the provider API
     * @param typeClassMap          A mapping of the API types to their representing classes
     * @return The created invocation handler
     */
    protected abstract InvocationHandler createApiInvocationHandler(Object providerApiObject, ConsumerApiDefinition consumerApiDefinition,
            DefinitionResolution definitionResolution, TypeClassMap typeClassMap);

}
