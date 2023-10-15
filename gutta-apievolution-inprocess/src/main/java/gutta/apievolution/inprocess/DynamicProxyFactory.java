package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.NoSuchElementException;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;

class DynamicProxyFactory implements ProxyFactory {

    @Override
    @SuppressWarnings("unchecked")
    public <T> T createProxy(Object providerApi, ConsumerApiDefinition consumerApiDefinition, Class<T> consumerApiType) {
        Class<?>[] implementedInterfaces = new Class<?>[] { consumerApiType };
        ApiInvocationHandler invocationHandler = new ApiInvocationHandler(consumerApiDefinition);

        return (T) Proxy.newProxyInstance(this.getClass().getClassLoader(), implementedInterfaces, invocationHandler);
    }

    private static class ApiInvocationHandler implements InvocationHandler {

        private final ConsumerApiDefinition consumerApiDefinition;
        
        ApiInvocationHandler(ConsumerApiDefinition consumerApiDefinition) {
            this.consumerApiDefinition = consumerApiDefinition;
        }
        
        @Override
        public Object invoke(Object proxy, Method method, Object[] arguments) {
            String methodName = method.getName();
            ConsumerOperation consumerOperation = this.consumerApiDefinition.resolveOperation(methodName).orElseThrow(NoSuchElementException::new);
            
            // TODO Auto-generated method stub
            return null;
        }

    }

}
