package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractMethodMappingStrategy;
import gutta.apievolution.inprocess.ApiMethodInvoker;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.TypeMappingStrategy;

import java.lang.reflect.Method;

class ObjectMappingMethodMappingStrategy extends AbstractMethodMappingStrategy {

    ObjectMappingMethodMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeToClassMap) {
        super(consumerApiDefinition, definitionResolution, typeToClassMap);
    }

    @Override
    protected ApiMethodInvoker createMethodInvoker(Method consumerMethod, ConsumerOperation consumerOperation, Method providerMethod,
            ProviderOperation providerOperation, TypeMappingStrategy typeMappingStrategy) {
        // TODO Auto-generated method stub
        return null;
    }

}
