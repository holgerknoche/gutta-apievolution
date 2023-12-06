package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.RecordType;
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
import java.util.Optional;

import static gutta.apievolution.inprocess.objectmapping.ImplementorSupport.determineImplementorOf;

class ObjectMappingMethodMappingStrategy extends AbstractMethodMappingStrategy {

    ObjectMappingMethodMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeToClassMap) {
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

        Class<?> declaredResultType = consumerMethod.getReturnType();        
        RecordType<?, ?, ?> modeledResultType = (RecordType<?, ?, ?>) this.typeToClassMap.classToType(declaredResultType);
        
        Optional<Class<?>> resultTypeImplementor = determineImplementorOf(declaredResultType);
        if (modeledResultType.isConcrete() && !resultTypeImplementor.isPresent()) {
            throw new InvalidApiException(
                    "Consumer result type '" + declaredResultType + "' of method '" + consumerMethod + "' is abstract and does not specify an implementor.");
        }
    }

    @Override
    protected void assertValidProviderMethod(Method providerMethod) {
        super.assertValidProviderMethod(providerMethod);

        Class<?> declaredParameterType = providerMethod.getParameters()[0].getType();
        Optional<Class<?>> parameterTypeImplementor = determineImplementorOf(declaredParameterType);
        if (!parameterTypeImplementor.isPresent()) {
            throw new InvalidApiException("Provider parameter type '" + declaredParameterType + "' of method '" + providerMethod +
                    "' is abstract and does not specify an implementor.");
        }
    }

}
