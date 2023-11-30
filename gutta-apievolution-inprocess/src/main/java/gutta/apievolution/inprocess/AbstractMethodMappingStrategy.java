package gutta.apievolution.inprocess;

import java.lang.reflect.Method;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.resolution.DefinitionResolution;

public abstract class AbstractMethodMappingStrategy implements MethodMappingStrategy {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final TypeClassMap typeToClassMap;

    protected AbstractMethodMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            TypeClassMap typeToClassMap) {
        
        this.consumerApiDefinition = consumerApiDefinition;
        this.definitionResolution = definitionResolution;
        this.typeToClassMap = typeToClassMap;
    }

    @Override
    public ApiMethodInvoker createMethodInvoker(Class<?> apiType, Method method, TypeMappingStrategy typeMappingStrategy) {
        // Perform specific assertions on the consumer method
        this.assertValidConsumerMethod(method);
        
        // Find the corresponding consumer and provider operations (in the model) for
        // the invoked method
        ConsumerOperation consumerOperation = this.findConsumerOperationFor(method);
        ProviderOperation providerOperation = this.definitionResolution.mapConsumerOperation(consumerOperation);

        // Find and validate the method representing the provider operation on the API
        // object
        Method providerMethod = this.findProviderMethodFor(providerOperation, apiType);
        this.assertValidProviderMethod(providerMethod);

        return this.createMethodInvoker(method, consumerOperation, providerMethod, providerOperation, typeMappingStrategy);
    }

    protected abstract ApiMethodInvoker createMethodInvoker(Method consumerMethod, ConsumerOperation consumerOperation,
            Method providerMethod, ProviderOperation providerOperation, TypeMappingStrategy typeMappingStrategy);

    private ConsumerOperation findConsumerOperationFor(Method method) {
        String methodName = method.getName();
        return this.consumerApiDefinition.resolveOperation(methodName)
                .orElseThrow(() -> new InvalidApiException("No matching API operation for method '" + method + "'."));
    }

    private Method findProviderMethodFor(ProviderOperation providerOperation, Class<?> providerApiClass) {
        // Determine and validate the class representing the parameter type
        ProviderRecordType providerParameterType = providerOperation.getParameterType();
        Class<?> providerParameterClass = this.typeToClassMap.providerTypeToClass(providerParameterType);

        // Attempt to find a method with the expected signature
        String providerMethodName = providerOperation.getInternalName();
        Method providerMethod;

        try {
            providerMethod = providerApiClass.getMethod(providerMethodName, providerParameterClass);
        } catch (NoSuchMethodException e) {
            throw new InvalidApiException("No method named '" + providerMethodName + "' on API '" + providerApiClass.getName() + "'.", e);
        }

        // Assert that the return type matches the expectation
        ProviderRecordType providerResultType = providerOperation.getReturnType();
        Class<?> providerResultClass = this.typeToClassMap.providerTypeToClass(providerResultType);
        if (!providerResultClass.equals(providerMethod.getReturnType())) {
            throw new InvalidApiException("Method '" + providerMethod + "' has an unexpected return type.");
        }
        
        // Perform specific assertions on the provider method
        this.assertValidProviderMethod(providerMethod);

        return providerMethod;
    }
    
    protected void assertValidConsumerMethod(Method consumerMethod) {
        // Assert that a consumer method has just a single parameter type
        if (consumerMethod.getParameterCount() != 1) {
            throw new InvalidApiException("Consumer method '" + consumerMethod + "' has an invalid number of parameters.");
        }
    }
        
    protected void assertValidProviderMethod(Method providerMethod) {
        // Assert that a provider method has just a single parameter type
        if (providerMethod.getParameterCount() != 1) {
            throw new InvalidApiException("Provider method '" + providerMethod + "' has an invalid number of parameters.");
        }        
    }

}
