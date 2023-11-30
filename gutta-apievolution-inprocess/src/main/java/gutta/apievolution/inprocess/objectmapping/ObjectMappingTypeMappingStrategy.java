package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractTypeMappingStrategy;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.ValueMapper;

class ObjectMappingTypeMappingStrategy extends AbstractTypeMappingStrategy {

    ObjectMappingTypeMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeToClassMap) {
        super(consumerApiDefinition, definitionResolution, typeToClassMap);
    }

    @Override
    protected ValueMapper createMapperFor(Class<?> javaClass) {
        // TODO Auto-generated method stub
        return null;
    }

}
