package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.NoSuchElementException;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.resolution.DefinitionResolution;

class DynamicProxyFactory implements ProxyFactory {

    @Override
    @SuppressWarnings("unchecked")
    public <T> T createProxy(Object providerApi, ResolvedConsumerApiDefinition consumerApiDefinition, Class<T> consumerApiType) {
        Class<?>[] implementedInterfaces = new Class<?>[] { consumerApiType };
        ApiInvocationHandler invocationHandler = new ApiInvocationHandler(consumerApiDefinition);

        return (T) Proxy.newProxyInstance(this.getClass().getClassLoader(), implementedInterfaces, invocationHandler);
    }

    private static class ApiInvocationHandler implements InvocationHandler {

        private final ConsumerApiDefinition consumerApiDefinition;
        
        private final DefinitionResolution definitionResolution;
        
        ApiInvocationHandler(ResolvedConsumerApiDefinition resolvedConsumerApiDefinition) {
            this.consumerApiDefinition = resolvedConsumerApiDefinition.getConsumerApiDefinition();
            this.definitionResolution = resolvedConsumerApiDefinition.getDefinitionResolution();
        }
        
        @Override
        public Object invoke(Object proxy, Method method, Object[] arguments) {
            String methodName = method.getName();
            ConsumerOperation consumerOperation = this.consumerApiDefinition.resolveOperation(methodName).orElseThrow(NoSuchElementException::new);
            ProviderOperation providerOperation = this.definitionResolution.mapConsumerOperation(consumerOperation);                        
            
            // TODO Auto-generated method stub
            return null;
        }

    }
    
    private static class RecordInvocationHandler implements InvocationHandler {

        @Override
        public Object invoke(Object proxy, Method method, Object[] arguments) {
            // TODO Auto-generated method stub
            return null;
        }
        
    }

}
