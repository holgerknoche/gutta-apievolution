package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.lang.reflect.Method;

/**
 * Abstract implementation of a {@link MethodMappingStrategy} that provides common functionality.
 */
public abstract class AbstractMethodMappingStrategy implements MethodMappingStrategy {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final TypeClassMap typeToClassMap;

    /**
     * Creates a new method mapping strategy using the given data.
     * 
     * @param consumerApiDefinition The consumer API definition to use
     * @param definitionResolution  The resolution of the consumer API to the provider API
     * @param typeToClassMap        A mapping of the API types to the classes representing them
     */
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

    /**
     * Creates a method invoker using the given data.
     * 
     * @param consumerMethod      The invoked consumer method
     * @param consumerOperation   The corresponding consumer operation
     * @param providerMethod      The invoked provider method
     * @param providerOperation   The corresponding provider operation
     * @param typeMappingStrategy The type mapping strategy to use
     * @return The created method invoker
     */
    protected abstract ApiMethodInvoker createMethodInvoker(Method consumerMethod, ConsumerOperation consumerOperation, Method providerMethod,
            ProviderOperation providerOperation, TypeMappingStrategy typeMappingStrategy);

    private ConsumerOperation findConsumerOperationFor(Method method) {
        String methodName = method.getName();
        return this.consumerApiDefinition.resolveOperation(methodName)
                .orElseThrow(() -> new InvalidApiException("No matching API operation for method '" + method + "'."));
    }

    private Method findProviderMethodFor(ProviderOperation providerOperation, Class<?> providerApiClass) {
        // Determine and validate the class representing the parameter type
        ProviderRecordType providerParameterType = providerOperation.getParameterType();
        Class<?> providerParameterClass = this.typeToClassMap.typeToClass(providerParameterType);

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
        Class<?> providerResultClass = this.typeToClassMap.typeToClass(providerResultType);
        if (!providerResultClass.equals(providerMethod.getReturnType())) {
            throw new InvalidApiException("Method '" + providerMethod + "' has an unexpected return type.");
        }

        // Perform specific assertions on the provider method
        this.assertValidProviderMethod(providerMethod);

        return providerMethod;
    }

    /**
     * Asserts that the given consumer method is valid. This method can be overriden by specific strategies, but must call the super implementation to ensure
     * that all validations occur.
     * 
     * @param consumerMethod The consumer method to check
     */
    protected void assertValidConsumerMethod(Method consumerMethod) {
        // Assert that a consumer method has just a single parameter type
        if (consumerMethod.getParameterCount() != 1) {
            throw new InvalidApiException("Consumer method '" + consumerMethod + "' has an invalid number of parameters.");
        }
    }

    /**
     * Asserts that the given provide method is valid. This method can be overriden by specific strategies, but must call the super implementation to ensure
     * that all validations occur.
     * 
     * @param providerMethod The provider method to check
     */
    protected void assertValidProviderMethod(Method providerMethod) {
        // Assert that a provider method has just a single parameter type
        if (providerMethod.getParameterCount() != 1) {
            throw new InvalidApiException("Provider method '" + providerMethod + "' has an invalid number of parameters.");
        }
    }

}
