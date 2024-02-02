package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractRecordTypeValueMapper;
import gutta.apievolution.inprocess.AbstractTypeMappingStrategy;
import gutta.apievolution.inprocess.AbstractValueMapperCreator;
import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.InvalidApiException;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.ValueMapper;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

class ObjectMappingTypeMappingStrategy extends AbstractTypeMappingStrategy {

    ObjectMappingTypeMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeToClassMap) {
        super(consumerApiDefinition, definitionResolution, typeToClassMap);
    }

    @Override
    protected ValueMapper createMapperFor(Class<?> javaClass) {
        return new ValueMapperCreator(this).createMapperForClass(javaClass);
    }

    private static class ValueMapperCreator extends AbstractValueMapperCreator<ObjectMappingTypeMappingStrategy> {

        public ValueMapperCreator(ObjectMappingTypeMappingStrategy typeMappingStrategy) {
            super(typeMappingStrategy);
        }

        @Override
        protected ValueMapper createRecordValueMapper(RecordType<?, ?, ?> type, Class<?> representingClass, Map<Method, FieldMapper> fieldMappers) {
            // Polymorphic mapping is based on the source type, since polymorphic dispatch
            // is based on the runtime type of the source object
            RecordType<?, ?, ?> sourceType = this.determineOpposingType(type);

            if (sourceType.hasSubTypes()) {
                @SuppressWarnings("unchecked")
                Set<RecordType<?, ?, ?>> concreteSubtypes = (Set<RecordType<?, ?, ?>>) sourceType.collectAllSubtypes(RecordType::isConcrete);

                Map<Class<?>, AbstractRecordTypeValueMapper> subtypeMappers = new HashMap<>(concreteSubtypes.size());
                for (RecordType<?, ?, ?> sourceSubtype : concreteSubtypes) {
                    RecordType<?, ?, ?> targetSubtype = this.determineOpposingType(sourceSubtype);
                    if (targetSubtype == null) {
                        // The mapping from provider to consumer we may have a partial mapping
                        continue;
                    }
                    
                    AbstractRecordTypeValueMapper subtypeMapper = (AbstractRecordTypeValueMapper) this.createMapperForType(targetSubtype);

                    // Determine the actual source runtime type that will represent the subtype
                    Class<?> sourceRepresentationCandidate = this.getTypeToClassMap().typeToClass(sourceSubtype);
                    Class<?> sourceTypeRepresentation = this.determineAppropriateTypeFor(sourceSubtype, sourceRepresentationCandidate);
                    subtypeMappers.put(sourceTypeRepresentation, subtypeMapper);
                }

                return new PolymorphicRecordTypeValueMapper(representingClass, subtypeMappers);
            } else {
                return new RecordTypeValueMapper(type, representingClass, fieldMappers);
            }
        }

        @Override
        protected Optional<Method> findTargetAccessor(Field<?, ?> targetField, Class<?> targetClass) {
            return this.findWriteAccessorForField(targetField, targetClass);
        }

        @Override
        protected void validateTargetAccessor(Method accessor, Field<?, ?> targetField) {
            // Assert that the accessor has a void return type
            if (!accessor.getReturnType().equals(void.class)) {
                throw new InvalidApiException("Accessor '" + accessor + "' has an unexpected return type (expected 'void'.");
            }
        }

        @Override
        protected Optional<Method> findSourceAccessor(Field<?, ?> sourceField, Class<?> sourceClass) {
            return this.findReadAccessorForField(sourceField, sourceClass);
        }

        @Override
        protected Class<?> determineAppropriateTypeFor(RecordType<?, ?, ?> type, Class<?> representingClass) {
            if (type.isConcrete()) {
                return ImplementorSupport.determineMandatoryImplementorOf(representingClass);
            } else {
                return representingClass;
            }
        }

    }

}
