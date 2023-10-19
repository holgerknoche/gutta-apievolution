package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.NoSuchElementException;

class DynamicProxyFactory implements ProxyFactory {

    @Override
    @SuppressWarnings("unchecked")
    public <T> T createProxy(Object providerApi, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            TypeToClassMap typeToClassMap, Class<T> consumerApiType) {
        Class<?>[] implementedInterfaces = new Class<?>[] { consumerApiType };
        ApiInvocationHandler invocationHandler = new ApiInvocationHandler(providerApi, consumerApiDefinition, definitionResolution,
                typeToClassMap);

        return (T) Proxy.newProxyInstance(this.getClass().getClassLoader(), implementedInterfaces, invocationHandler);
    }

    private static class ApiInvocationHandler implements InvocationHandler {

        private final Object providerApi;

        private final ConsumerApiDefinition consumerApiDefinition;

        private final DefinitionResolution definitionResolution;

        private final TypeToClassMap typeToClassMap;

        ApiInvocationHandler(Object providerApi, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
                TypeToClassMap typeToClassMap) {
            this.providerApi = providerApi;
            this.consumerApiDefinition = consumerApiDefinition;
            this.definitionResolution = definitionResolution;
            this.typeToClassMap = typeToClassMap;
        }

        @Override
        public Object invoke(Object proxy, Method method, Object[] arguments) {
            // Map the consumer operation to the provider operation (in the model)
            String methodName = method.getName();
            ConsumerOperation consumerOperation = this.consumerApiDefinition.resolveOperation(methodName)
                    .orElseThrow(NoSuchElementException::new);
            ProviderOperation providerOperation = this.definitionResolution.mapConsumerOperation(consumerOperation);

            // Find the appropriate method for the provider operation on the API object
            ProviderRecordType providerParameterType = providerOperation.getParameterType();
            Class<?> providerParameterClass = this.typeToClassMap.providerRecordTypeToClass(providerParameterType);
            if (!providerParameterClass.isInterface()) {
                throw new InvalidApiException("Parameter type '" + providerParameterClass + "' is not an interface.");
            }
            
            ProviderRecordType providerResultType = providerOperation.getReturnType();
            Class<?> providerResultClass = this.typeToClassMap.providerRecordTypeToClass(providerResultType);
            
            Class<?> providerApiClass = this.providerApi.getClass();
            String providerMethodName = providerOperation.getInternalName();
            Method providerMethod;
            
            try {
                providerMethod = providerApiClass.getMethod(providerMethodName, providerParameterClass);                
            } catch (NoSuchMethodException e) {
                throw new InvalidApiException("Missing method '" + providerMethodName + "' on API '" + providerApiClass.getName() + "'." , e);
            }
            
            if (!providerResultClass.equals(providerMethod.getReturnType())) {
                throw new InvalidApiException("Method '" + providerMethod + "' has an unexpected return type.");
            }
            
            Class<?>[] parameterTypes = new Class<?>[] {providerParameterClass};
            RecordInvocationHandler parameterInvocationHandler = new RecordInvocationHandler();
            Object parameterProxy = Proxy.newProxyInstance(this.getClass().getClassLoader(), parameterTypes, parameterInvocationHandler);
            
            Object providerResult;
            try {
                providerResult = providerMethod.invoke(this.providerApi, parameterProxy);
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                // TODO Use an appropriate exception
                // TODO Map declared exceptions
                throw new RuntimeException(e);
            }
            
            if (providerResult == null) {
                // No mapping necessary when the result is null
                return null;
            } else {
                // Map the result to the consumer's expectation
                ConsumerRecordType consumerResultType = consumerOperation.getReturnType();
                Class<?> consumerResultClass = this.typeToClassMap.consumerRecordTypeToClass(consumerResultType);

                Class<?>[] resultTypes = new Class<?>[] {consumerResultClass};
                RecordInvocationHandler resultInvocationHandler = new RecordInvocationHandler();
                Object resultProxy = Proxy.newProxyInstance(this.getClass().getClassLoader(), resultTypes, resultInvocationHandler);
            
                return resultProxy;
            }
        }

    }

    private static class RecordInvocationHandler implements InvocationHandler {

        @Override
        public Object invoke(Object proxy, Method method, Object[] arguments) {
            // TODO Auto-generated method stub
            return null;
        }

    }

    static class InvalidApiException extends RuntimeException {

        public InvalidApiException(String message) {
            super(message);
        }

        public InvalidApiException(String message, Throwable cause) {
            super(message, cause);
        }

    }

}
