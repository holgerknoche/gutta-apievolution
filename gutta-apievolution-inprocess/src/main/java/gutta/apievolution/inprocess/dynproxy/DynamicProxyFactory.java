package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.ProxyFactory;
import gutta.apievolution.inprocess.UDTToClassMap;

import java.lang.reflect.Proxy;

// Comments on dynamic proxies (esp. vs. explicit object conversion)
// - Proxy creation is not free, though may be cheaper if few attributes are actually read
// - Interface requirement is cumbersome for exceptions
// - equals() is either identity or inherited from the implementation. In the latter case,
//   an object that is indistinguishable from the consumer perspective may not be equal due
//   to fields only visible to the provider

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
