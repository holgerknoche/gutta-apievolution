package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.ProxyFactory;
import gutta.apievolution.inprocess.UDTToClassMap;

import java.lang.reflect.Proxy;

public class DynamicProxyFactory implements ProxyFactory {

    @Override
    @SuppressWarnings("unchecked")
    public <T> T createProxy(Object providerApi, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap, Class<T> consumerApiType) {
        Class<?>[] implementedInterfaces = new Class<?>[] { consumerApiType };
        ApiInvocationHandler invocationHandler = new ApiInvocationHandler(providerApi, consumerApiDefinition, definitionResolution,
                typeToClassMap);

        return (T) Proxy.newProxyInstance(this.getClass().getClassLoader(), implementedInterfaces, invocationHandler);
    }

}
