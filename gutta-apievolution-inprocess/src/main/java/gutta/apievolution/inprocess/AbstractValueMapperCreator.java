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
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

import static gutta.apievolution.core.apimodel.Type.mostSpecificTypeOf;

/**
 * Abstract supertype for a creator that produces {@link ValueMapper ValueMappers} that provides common functionality.
 * 
 * @param <T> The employed type mapping strategy
 */
public abstract class AbstractValueMapperCreator<T extends AbstractTypeMappingStrategy> implements TypeVisitor<ValueMapper> {

    private final T typeMappingStrategy;

    /**
     * Creates a new mapper creator that uses the given type mapping strategy.
     * 
     * @param typeMappingStrategy The type mapping strategy to use
     */
    protected AbstractValueMapperCreator(T typeMappingStrategy) {
        this.typeMappingStrategy = typeMappingStrategy;
    }

    /**
     * Finds the API type that is represented by the given class.
     * 
     * @param javaClass The class representing the type
     * @return The type represented by the class or {@code null} if the class does not represent a type
     */
    protected Type findTypeRepresentedBy(Class<?> javaClass) {
        return this.typeMappingStrategy.findTypeRepresentedBy(javaClass);
    }

    /**
     * Returns the definition resolution of the used consumer API against the provider API.
     * 
     * @return see above
     */
    protected DefinitionResolution getDefinitionResolution() {
        return this.typeMappingStrategy.getDefinitionResolution();
    }

    /**
     * Returns the employed mapping of API types their representing classes.
     * 
     * @return see above
     */
    protected TypeClassMap getTypeToClassMap() {
        return this.typeMappingStrategy.getTypeToClassMap();
    }

    /**
     * Denotes whether the given type is a provider type.
     * 
     * @param type The type to check
     * @return {@code True} if the type is a provider type, {@code false} otherwise
     */
    protected static boolean isProviderType(Type type) {
        return (type instanceof UserDefinedType) && ((UserDefinedType<?>) type).isProviderType();
    }

    /**
     * Finds a read accessor for the given field on the given type. A method is considered a read accessor for a field if and only if:
     * 
     * <ul>
     * <li>it has no parameters</li>
     * <li>it is named {@code getXyz} or {@code xyz}, where {@code xyz} is the field's internal name
     * <li>it returns the type representing the field's type</li>
     * </ul>
     * 
     * @param field The field for which an accessor is required
     * @param type  The type on which the accessor is required
     * @return The appropriate accessor method, if it exits
     */
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

    /**
     * Finds a write accessor for the given field on the given type. A method is considered a write accessor for a field if and only if:
     * 
     * <ul>
     * <li>it has one parameter of the type representing the field's type</li>
     * <li>it is named {@code setXyz} or {@code xyz}, where {@code xyz} is the field's internal name
     * <li>it has return type {@code void}
     * </ul>
     * 
     * @param field The field for which an accessor is required
     * @param type  The type on which the accessor is required
     * @return The appropriate accessor method, if it exits
     */
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

    /**
     * Determines the opposing type of the given type, i.e., the consumer type for a provider type or vice versa.
     * 
     * @param <X>  The expected type of API type
     * @param type The type to map
     * @return The opposing type
     */
    protected <X extends Type> X determineOpposingType(UserDefinedType<?> type) {
        return this.getDefinitionResolution().mapType(type);
    }

    /**
     * Determines the opposing enum member of the given enum member, i.e., the consumer member for a provider member or vice versa.
     * 
     * @param type The enum member to map
     * @return The opposing enum member
     */
    protected EnumMember<?, ?> determineOpposingMember(EnumMember<?, ?> member) {
        if (member instanceof ConsumerEnumMember) {
            return this.getDefinitionResolution().mapConsumerEnumMember((ConsumerEnumMember) member);
        } else {
            return this.getDefinitionResolution().mapProviderEnumMember((ProviderEnumMember) member);
        }
    }

    /**
     * Creates a value mapper for the given type.
     * 
     * @param type The type to create a mapper for
     * @return The created mapper
     */
    public ValueMapper createMapperForClass(Class<?> type) {
        Type sourceType = this.findTypeRepresentedBy(type);
        if (sourceType == null) {
            // No mappers for types that do not represent any API type
            return null;
        }

        if (!sourceType.isUserDefined()) {
            // When invoked with a class, the input class must be a user-defined type.
            // Especially lists would be tricky due to type erasure
            throw new IllegalArgumentException("Unmappable input class '" + type + "'.");
        }
        Type targetType = this.getDefinitionResolution().mapType(sourceType);
                
        if (targetType != null) {
            // Mappers are created based on the target type (i.e., the reader's expectation)
            return this.createMapperForType(targetType);
        } else {
            return this.createMapperForUnrepresentableType(sourceType);
        }
    }

    /**
     * Creates a value mapper for the given API type.
     * 
     * @param type The API type to create a mapper for
     * @return The created mapper
     */
    protected ValueMapper createMapperForType(Type type) {
        return type.accept(this);
    }

    /**
     * Creates a mapper for the given unrepresentable type.
     * 
     * @param type The unrepresentable type to create a mapper for
     * @return An appropriate value mapper or {@code null} if no mapper can be created
     */
    protected ValueMapper createMapperForUnrepresentableType(Type type) {
        if (type instanceof RecordType) {
            RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;
            
            if (recordType.getSuperTypes().isEmpty()) {
                // If the type does not have any supertypes, use the default behavior
                // for unrelated types
                return null;
            }
            
            // If the type is a record type, there could be a supertype that can be mapped
            // and that might specify a behavior for unmapped types
            Type mappedSupertype = this.findMostSpecificMappedSupertypeOf(recordType);
            if (mappedSupertype == null) {
                return null;
            }
            
            Class<?> representingClass = this.getTypeToClassMap().typeToClass(mappedSupertype);
            return new UnrepresentableRecordTypeMapper(representingClass);
        } else {
            // For non-record types, treat this case as if the representing class does not
            // represent an API type
            return null;
        }
    }
    
    private RecordType<?, ?, ?> findMostSpecificMappedSupertypeOf(RecordType<?, ?, ?> type) {
        Set<RecordType<?, ?, ?>> mappedSupertypes = new HashSet<>();
        this.collectMappedSupertypesOf(type, mappedSupertypes::add);
        
        return mostSpecificTypeOf(mappedSupertypes);
    }
    
    private void collectMappedSupertypesOf(RecordType<?, ?, ?> type, Consumer<RecordType<?, ?, ?>> collector) {
        RecordType<?, ?, ?> mappedType = this.getDefinitionResolution().mapType(type);
        if (mappedType != null) {
            collector.accept(mappedType);
        } else {
            type.getSuperTypes().forEach(superType -> this.collectMappedSupertypesOf(superType, collector));
        }
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

        return new EnumTypeValueMapper(targetClass, memberMap);
    }

    @Override
    public ValueMapper handleRecordType(RecordType<?, ?, ?> targetType) {
        RecordType<?, ?, ?> sourceType = this.determineOpposingType(targetType);
        Class<?> sourceClass = this.getTypeToClassMap().typeToClass(sourceType);
        Class<?> targetClass = this.determineAppropriateTypeFor(targetType, this.getTypeToClassMap().typeToClass(targetType));

        // Only the provider must deal with unmapped elements
        boolean allowUnmappedElements = isProviderType(targetType);

        Map<Method, FieldMapper> fieldMappers = new HashMap<>();
        targetType.getFields().forEach(field -> this.registerMapperForField(field, sourceClass, targetClass, fieldMappers, allowUnmappedElements));

        return this.createRecordValueMapper(targetType, targetClass, fieldMappers);
    }

    /**
     * Creates a record value mapper for the given type.
     * 
     * @param type              The record type
     * @param representingClass The class representing the record type
     * @param fieldMappers      The field mappers for the record type
     * @return The created value mapper
     */
    protected abstract ValueMapper createRecordValueMapper(RecordType<?, ?, ?> type, Class<?> representingClass, Map<Method, FieldMapper> fieldMappers);

    private void registerMapperForField(Field<?, ?> targetField, Class<?> sourceClass, Class<?> targetClass, Map<Method, FieldMapper> fieldMappers,
            boolean allowUnmappedElements) {
        // Find a potential accessor method on the target class
        Method accessor = this.findTargetAccessor(targetField, targetClass)
                .orElseThrow(() -> new InvalidApiException("No accessor for field '" + targetField + "' on class '" + targetClass.getName() + "'."));

        FieldMapper fieldMapper = this.createMapperForField(targetField, sourceClass, targetClass, allowUnmappedElements);
        fieldMappers.put(accessor, fieldMapper);
    }

    /**
     * Finds the target accessor for the given field on the given type.
     * 
     * @param targetField The field to find the accessor for
     * @param targetClass The class on which to find the accessor
     * @return The accessor method, if it exists
     */
    protected abstract Optional<Method> findTargetAccessor(Field<?, ?> targetField, Class<?> targetClass);

    /**
     * Validates the target accessor for the given field.
     * 
     * @param accessor    The accessor to validate
     * @param targetField The field for which the accessor is used
     */
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

    /**
     * Finds the source accessor for the given field on the given type.
     * 
     * @param sourceField The field to find the accessor for
     * @param sourceClass The class on which to find the accessor
     * @return The accessor method, if it exists
     */
    protected abstract Optional<Method> findSourceAccessor(Field<?, ?> sourceField, Class<?> sourceClass);

    /**
     * Determines the actual type to use for the given type. This operation can be overridden by subtypes to replace, for instance, interfaces by their
     * implementing classes.
     * 
     * @param type              The record type in question
     * @param representingClass The class representing the type
     * @return The actual type to use (can be the same as the input)
     */
    protected Class<?> determineAppropriateTypeFor(RecordType<?, ?, ?> type, Class<?> representingClass) {
        return representingClass;
    }        

}
