package gutta.apievolution.inprocess;

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
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public abstract class AbstractValueMapperCreator<T extends AbstractTypeMappingStrategy> implements TypeVisitor<ValueMapper> {

    private final T typeMappingStrategy;

    protected AbstractValueMapperCreator(T typeMappingStrategy) {
        this.typeMappingStrategy = typeMappingStrategy;
    }

    protected Type findTypeMatching(Class<?> javaClass) {
        return this.typeMappingStrategy.findTypeMatching(javaClass);
    }

    protected DefinitionResolution getDefinitionResolution() {
        return this.typeMappingStrategy.getDefinitionResolution();
    }

    protected TypeClassMap getTypeToClassMap() {
        return this.typeMappingStrategy.getTypeToClassMap();
    }

    protected static boolean isProviderType(Type type) {
        return (type instanceof UserDefinedType) && ((UserDefinedType<?>) type).isProviderType();
    }

    protected Optional<Method> findReadAccessorForField(Field<?, ?> field, Class<?> type) {
        String fieldName = field.getInternalName();

        // Try a "get" method first...
        Optional<Method> optionalGetter = findMethod(type, getterNameFor(fieldName));
        if (optionalGetter.isPresent()) {
            return optionalGetter;
        }

        // ..., then try a method with the exact name (such as in records)
        Optional<Method> optionalAccessor = findMethod(type, fieldName);
        if (optionalAccessor.isPresent()) {
            return optionalAccessor;
        }

        return Optional.empty();
    }

    protected Optional<Method> findWriteAccessorForField(Field<?, ?> field, Class<?> type) {
        String fieldName = field.getInternalName();
        Class<?> fieldType = this.getTypeToClassMap().typeToClass(field.getType());

        // Try a "set" method first...
        Optional<Method> optionalSetter = findMethod(type, setterNameFor(fieldName), fieldType);
        if (optionalSetter.isPresent()) {
            return optionalSetter;
        }

        // ..., then try a method with the exact name (such as in records)
        Optional<Method> optionalAccessor = findMethod(type, fieldName, fieldType);
        if (optionalAccessor.isPresent()) {
            return optionalAccessor;
        }

        return Optional.empty();
    }

    private static Optional<Method> findMethod(Class<?> type, String name, Class<?>... argumentTypes) {
        try {
            return Optional.of(type.getMethod(name, argumentTypes));
        } catch (NoSuchMethodException e) {
            return Optional.empty();
        }
    }

    private static String getterNameFor(String fieldName) {
        char firstChar = fieldName.charAt(0);
        String remainder = (fieldName.length() == 1) ? "" : fieldName.substring(1);

        return "get" + Character.toUpperCase(firstChar) + remainder;
    }

    private static String setterNameFor(String fieldName) {
        char firstChar = fieldName.charAt(0);
        String remainder = (fieldName.length() == 1) ? "" : fieldName.substring(1);

        return "set" + Character.toUpperCase(firstChar) + remainder;
    }

    protected <X extends Type> X determineOpposingType(UserDefinedType<?> type) {
        return this.getDefinitionResolution().mapType(type);
    }

    protected EnumMember<?, ?> determineOpposingMember(EnumMember<?, ?> member) {
        if (member instanceof ConsumerEnumMember) {
            return this.getDefinitionResolution().mapConsumerEnumMember((ConsumerEnumMember) member);
        } else {
            return this.getDefinitionResolution().mapProviderEnumMember((ProviderEnumMember) member);
        }
    }

    public ValueMapper createMapperForClass(Class<?> type) {
        Type sourceType = this.findTypeMatching(type);
        if (sourceType == null) {
            // No mappers for unmapped types
            return null;
        }

        if (!sourceType.isUserDefined()) {
            // When invoked with a class, the input class must be a user-defined type.
            // Especially lists would be tricky due to type erasure
            throw new IllegalArgumentException("Unmappable input class '" + type + "'.");
        }
        Type targetType = this.getDefinitionResolution().mapType(sourceType);

        // Mappers are created based on the target type (i.e., the reader's expectation)
        return this.createMapperForType(targetType);
    }

    protected ValueMapper createMapperForType(Type type) {
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
        Class sourceClass = this.getTypeToClassMap().typeToClass(sourceType);
        Class targetClass = this.getTypeToClassMap().typeToClass(targetType);

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
        Class<?> sourceClass = this.getTypeToClassMap().typeToClass(sourceType);
        Class<?> targetClass = this.determineAppropriateTypeFor(this.getTypeToClassMap().typeToClass(targetType));

        // Only the provider must deal with unmapped elements
        boolean allowUnmappedElements = isProviderType(targetType);

        Map<Method, FieldMapper> fieldMappers = new HashMap<>();
        targetType.getFields().forEach(field -> this.registerMapperForField(field, sourceClass, targetClass, fieldMappers, allowUnmappedElements));

        return this.createRecordValueMapper(targetClass, fieldMappers);
    }

    protected abstract ValueMapper createRecordValueMapper(Class<?> targetClass, Map<Method, FieldMapper> fieldMappers);

    private void registerMapperForField(Field<?, ?> targetField, Class<?> sourceClass, Class<?> targetClass, Map<Method, FieldMapper> fieldMappers,
            boolean allowUnmappedElements) {
        // Find a potential accessor method on the target class
        Method accessor = this.findTargetAccessor(targetField, targetClass)
                .orElseThrow(() -> new InvalidApiException("No accessor for field '" + targetField + "' on class '" + targetClass.getName() + "'."));

        FieldMapper fieldMapper = this.createMapperForField(targetField, sourceClass, targetClass, allowUnmappedElements);
        fieldMappers.put(accessor, fieldMapper);
    }

    protected abstract Optional<Method> findTargetAccessor(Field<?, ?> targetField, Class<?> targetClass);

    protected abstract void validateTargetAccessor(Method accessor, Field<?, ?> targetField);

    private FieldMapper createMapperForField(Field<?, ?> targetField, Class<?> sourceClass, Class<?> targetClass, boolean allowUnmappedElements) {
        Field<?, ?> sourceField = this.getDefinitionResolution().mapField(targetField);

        if (sourceField == null) {
            if (allowUnmappedElements) {
                // Provider fields may be unmatched
                return new UnmatchedFieldMapper();
            } else {
                throw new InvalidApiException("Field '" + targetField + "' is unmapped, but must not be.");
            }
        }

        Method sourceAccessor = this.findSourceAccessor(sourceField, sourceClass)
                .orElseThrow(() -> new InvalidApiException("No accessor for field '" + sourceField + "' on class '" + sourceClass.getName() + "'."));

        Class<?> expectedClass = getTypeToClassMap().typeToClass(sourceField.getType());
        if (!expectedClass.equals(sourceAccessor.getReturnType())) {
            throw new InvalidApiException("Accessor '" + sourceAccessor + "' has an unexpected return type (expected '" + expectedClass + "'.");
        }

        ValueMapper valueMapper = this.createMapperForType(targetField.getType());
        return new ReflectiveFieldMapper(sourceAccessor, valueMapper);
    }

    protected abstract Optional<Method> findSourceAccessor(Field<?, ?> sourceField, Class<?> sourceClass);

    protected Class<?> determineAppropriateTypeFor(Class<?> type) {
        return type;
    }

}
