package gutta.apievolution.inprocess.dynproxy;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.BoundedListType;
import gutta.apievolution.core.apimodel.BoundedStringType;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UnboundedListType;
import gutta.apievolution.core.apimodel.UnboundedStringType;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.AbstractTypeMappingStrategy;
import gutta.apievolution.inprocess.InvalidApiException;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.ValueMapper;

class DynamicProxyTypeMappingStrategy extends AbstractTypeMappingStrategy {

    public DynamicProxyTypeMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            TypeClassMap typeClassMap) {

        super(consumerApiDefinition, definitionResolution, typeClassMap);
    }

    @Override
    protected ValueMapper createMapperFor(Class<?> type) {
        return new ValueMapperCreator().createMapperForClass(type);
    }

    private class ValueMapperCreator implements TypeVisitor<ValueMapper> {

        public ValueMapper createMapperForClass(Class<?> type) {
            Type sourceType = findTypeMatching(type);
            if (sourceType == null || (!sourceType.isUserDefined())) {
                // When invoked with a class, the input class must be a known user-defined type.
                // Especially lists would be tricky due to type erasure
                throw new IllegalArgumentException("Unmappable input class '" + type + "'.");
            }            
            Type targetType = getDefinitionResolution().mapType(sourceType);

            // Mappers are created based on the target type (i.e., the reader's expectation)
            return this.createMapperForType(targetType);
        }
        
        private ValueMapper createMapperForType(Type type) {
            return type.accept(this);
        }

        @Override
        public ValueMapper handleAtomicType(AtomicType atomicType) {
            return new BasicTypeValueMapper();
        }

        private ValueMapper handleStringType(StringType type) {
            return new BasicTypeValueMapper();
        }

        @Override
        public ValueMapper handleBoundedStringType(BoundedStringType boundedStringType) {
            return this.handleStringType(boundedStringType);
        }

        @Override
        public ValueMapper handleUnboundedStringType(UnboundedStringType unboundedStringType) {
            return this.handleStringType(unboundedStringType);
        }

        private ValueMapper handleListType(ListType listType) {
            Type elementType = listType.getElementType();
            ValueMapper elementMapper = elementType.accept(this);
            return new ListTypeValueMapper(elementMapper);
        }

        @Override
        public ValueMapper handleBoundedListType(BoundedListType boundedListType) {
            return this.handleListType(boundedListType);
        }

        @Override
        public ValueMapper handleUnboundedListType(UnboundedListType unboundedListType) {
            return this.handleListType(unboundedListType);
        }

        @Override
        @SuppressWarnings({ "rawtypes", "unchecked" })
        public ValueMapper handleEnumType(EnumType<?, ?, ?> targetType) {
            EnumType<?, ?, ?> sourceType = this.determineOpposingType(targetType);
            Class sourceClass = getTypeToClassMap().typeToClass(sourceType);
            Class targetClass = getTypeToClassMap().typeToClass(targetType);

            Map<Enum<?>, Enum<?>> memberMap = new HashMap<>();
            for (EnumMember<?, ?> targetMember : targetType.getDeclaredMembers()) {
                String targetName = targetMember.getInternalName();
                Enum<?> targetValue = Enum.valueOf(targetClass, targetName);

                EnumMember<?, ?> sourceMember = this.determineOpposingMember(targetMember);
                String sourceName = sourceMember.getInternalName();
                Enum<?> sourceValue = Enum.valueOf(sourceClass, sourceName);

                memberMap.put(sourceValue, targetValue);
            }

            return new EnumTypeValueMapper(memberMap);
        }

        @Override
        public ValueMapper handleRecordType(RecordType<?, ?, ?> targetType) {
            RecordType<?, ?, ?> sourceType = this.determineOpposingType(targetType);
            Class<?> sourceClass = getTypeToClassMap().typeToClass(sourceType);
            Class<?> targetClass = getTypeToClassMap().typeToClass(targetType);

            // Only the provider must deal with unmapped elements
            boolean allowUnmappedElements = isProviderType(targetType);

            Map<Method, FieldMapper> fieldMappers = new HashMap<>();
            targetType.getFields()
                    .forEach(field -> this.registerMapperForField(field, sourceClass, targetClass, fieldMappers, allowUnmappedElements));

            return new RecordTypeValueMapper(targetClass, fieldMappers);
        }

        private void registerMapperForField(Field<?, ?> targetField, Class<?> sourceClass, Class<?> targetClass,
                Map<Method, FieldMapper> fieldMappers, boolean allowUnmappedElements) {
            // Find a potential accessor method on the target class
            Method accessor = findAccessorForField(targetField, targetClass).orElseThrow(
                    () -> new InvalidApiException("No accessor for field '" + targetField + "' on class '" + targetClass.getName() + "'."));

            // Ensure that the accessor method has the expected type
            Type expectedType = targetField.getType();
            Class<?> expectedClass = getTypeToClassMap().typeToClass(expectedType);
            if (!expectedClass.equals(accessor.getReturnType())) {
                throw new InvalidApiException(
                        "Accessor '" + accessor + "' has an unexpected return type (expected '" + expectedClass + "'.");
            }

            FieldMapper fieldMapper = this.createMapperForField(targetField, sourceClass, targetClass, allowUnmappedElements);
            fieldMappers.put(accessor, fieldMapper);
        }

        private FieldMapper createMapperForField(Field<?, ?> targetField, Class<?> sourceClass, Class<?> targetClass,
                boolean allowUnmappedElements) {
            Field<?, ?> sourceField = getDefinitionResolution().mapField(targetField);

            if (sourceField == null) {
                if (allowUnmappedElements) {
                    // Provider fields may be unmatched
                    return new UnmatchedFieldMapper();
                } else {
                    throw new InvalidApiException("Field '" + targetField + "' is unmapped, but must not be.");
                }
            }
            
            Method sourceAccessor = findAccessorForField(sourceField, sourceClass).orElseThrow(
                    () -> new InvalidApiException("No accessor for field '" + sourceField + "' on class '" + sourceClass.getName() + "'."));

            Class<?> expectedClass = getTypeToClassMap().typeToClass(sourceField.getType());
            if (!expectedClass.equals(sourceAccessor.getReturnType())) {
                throw new InvalidApiException(
                        "Accessor '" + sourceAccessor + "' has an unexpected return type (expected '" + expectedClass + "'.");
            }

            ValueMapper valueMapper = this.createMapperForType(targetField.getType());
            return new ReflectiveFieldMapper(sourceAccessor, valueMapper);
        }

        private <T extends Type> T determineOpposingType(UserDefinedType<?> type) {
            return getDefinitionResolution().mapType(type);
        }

        private EnumMember<?, ?> determineOpposingMember(EnumMember<?, ?> member) {
            if (member instanceof ConsumerEnumMember) {
                return getDefinitionResolution().mapConsumerEnumMember((ConsumerEnumMember) member);
            } else {
                return getDefinitionResolution().mapProviderEnumMember((ProviderEnumMember) member);
            }
        }

    }

}
