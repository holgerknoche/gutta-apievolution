package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractTypeMappingStrategy;
import gutta.apievolution.inprocess.AbstractValueMapperCreator;
import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.InvalidApiException;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.ValueMapper;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.Optional;

class DynamicProxyTypeMappingStrategy extends AbstractTypeMappingStrategy {

    public DynamicProxyTypeMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeClassMap) {

        super(consumerApiDefinition, definitionResolution, typeClassMap);
    }

    @Override
    protected ValueMapper createMapperFor(Class<?> type) {
        return new ValueMapperCreator(this).createMapperForClass(type);
    }

    private static class ValueMapperCreator extends AbstractValueMapperCreator<DynamicProxyTypeMappingStrategy> {

        public ValueMapperCreator(DynamicProxyTypeMappingStrategy typeMappingStrategy) {
            super(typeMappingStrategy);
        }

        @Override
        protected Optional<Method> findSourceAccessor(Field<?, ?> sourceField, Class<?> sourceClass) {
            return this.findReadAccessorForField(sourceField, sourceClass);
        }

        @Override
        protected Optional<Method> findTargetAccessor(Field<?, ?> targetField, Class<?> targetClass) {
            return this.findReadAccessorForField(targetField, targetClass);
        }

        @Override
        protected void validateTargetAccessor(Method accessor, Field<?, ?> targetField) {
            // Ensure that the accessor method has the expected type
            Type expectedType = targetField.getType();
            Class<?> expectedClass = this.getTypeToClassMap().typeToClass(expectedType);
            if (!expectedClass.equals(accessor.getReturnType())) {
                throw new InvalidApiException("Accessor '" + accessor + "' has an unexpected return type (expected '" + expectedClass + "'.");
            }
        }

        @Override
        protected ValueMapper createRecordValueMapper(RecordType<?, ?, ?> type, Class<?> representingClass, Map<Method, FieldMapper> fieldMappers) {
            return new RecordTypeValueMapper(representingClass, fieldMappers);
        }

    }

}
