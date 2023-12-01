package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractMethodMappingStrategy;
import gutta.apievolution.inprocess.ApiMethodInvoker;
import gutta.apievolution.inprocess.InvalidApiException;
import gutta.apievolution.inprocess.ReflectiveMethodInvoker;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.TypeMappingStrategy;

import java.lang.reflect.Method;

class DynamicProxyMethodMappingStrategy extends AbstractMethodMappingStrategy {

    public DynamicProxyMethodMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            TypeClassMap typeToClassMap) {

        super(consumerApiDefinition, definitionResolution, typeToClassMap);
    }

    @Override
    protected ApiMethodInvoker createMethodInvoker(Method consumerMethod, ConsumerOperation consumerOperation, Method providerMethod,
            ProviderOperation providerOperation, TypeMappingStrategy typeMappingStrategy) {

        return new ReflectiveMethodInvoker(typeMappingStrategy, providerMethod);
    }

    @Override
    protected void assertValidConsumerMethod(Method consumerMethod) {
        super.assertValidConsumerMethod(consumerMethod);

        Class<?> resultType = consumerMethod.getReturnType();
        if (!resultType.isInterface()) {
            throw new InvalidApiException("Consumer result type '" + resultType + "' of method '" + consumerMethod + "' is not an interface.");
        }
    }

    @Override
    protected void assertValidProviderMethod(Method providerMethod) {
        super.assertValidProviderMethod(providerMethod);

        Class<?> parameterType = providerMethod.getParameters()[0].getType();
        if (!parameterType.isInterface()) {
            throw new InvalidApiException("Provider parameter type '" + parameterType + "' of method '" + providerMethod + "' is not an interface.");
        }
    }

}
