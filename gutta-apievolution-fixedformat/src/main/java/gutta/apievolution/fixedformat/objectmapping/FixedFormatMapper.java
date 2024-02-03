package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Consumer;

import static java.lang.reflect.Modifier.isAbstract;

/**
 * Simple fixed-format object-to-data mapper.
 */
public class FixedFormatMapper {
            
    private final ConcurrentMap<Class<?>, TypeMapper<?>> typeMappers = new ConcurrentHashMap<>();
            
    private synchronized TypeMapper<?> determineTypeMapperFor(Class<?> type) {
        TypeMapper<?> mapper = this.typeMappers.get(type);
        if (mapper != null) {
            return mapper;
        }
        
        return this.createTypeMapperFor(type, null, type);
    }
    
    private TypeMapper<?> createTypeMapperFor(Class<?> type, Type genericType, AnnotatedElement element) {
        TypeMapper<?> typeMapper;
        
        if (type.equals(int.class) || type.equals(Integer.class)) {
            typeMapper = new Int32Mapper();
        } else if (type.equals(String.class)) {
            MaxLength lengthAnnotation = element.getAnnotation(MaxLength.class);
            if (lengthAnnotation == null) {
                throw new InvalidRepresentationElementException("MaxLength annotation missing on element " + element + ".");
            }
            
            int fieldLength = lengthAnnotation.value();
            typeMapper = new StringMapper(fieldLength);
        } else if (type.equals(List.class)) {
            typeMapper = this.createTypeMapperForList((ParameterizedType) genericType, element);
        } else if (type.isEnum()) {
            typeMapper = new EnumMapper(type);
        } else {
            typeMapper = this.createTypeMapperForRecord(type);
        }
        
        if (typeMapper.isCacheable()) {
            this.typeMappers.put(type, typeMapper);
        }
            
        return typeMapper;
    }
        
    private TypeMapper<?> createTypeMapperForRecord(Class<?> type) {
        SubTypes subTypesAnnotation = type.getAnnotation(SubTypes.class);        
        if (subTypesAnnotation != null) {
            // If there are subtypes, we need a polymorphic type mapper
            Map<Integer, RecordTypeMapper> subTypeMappers = this.mappersForAllConcreteSubtypes(type);            
            return new PolymorphicRecordTypeMapper(type, subTypeMappers);
        } else {
            // Otherwise, create a regular record type mapper
            return this.createTypeMapperForConcreteRecord(type);
        }
    }
    
    private static Set<Class<?>> findAllConcreteSubtypesOf(Class<?> type) {
        Set<Class<?>> subTypes = new HashSet<>();
        
        collectConcreteSubtypesOf(type, subTypes);
        
        return subTypes;
    }
    
    private static void collectConcreteSubtypesOf(Class<?> type, Set<Class<?>> subTypes) {
        if (!isAbstract(type.getModifiers())) {
            subTypes.add(type);
        }
        
        SubTypes subTypesAnnotation = type.getAnnotation(SubTypes.class);
        if (subTypesAnnotation == null) {
            return;
        }
        
        for (Class<?> subType : subTypesAnnotation.value()) {
            collectConcreteSubtypesOf(subType, subTypes);
        }
    }
    
    private Map<Integer, RecordTypeMapper> mappersForAllConcreteSubtypes(Class<?> type) {
        // Find all concrete subtypes, possibly including the type itself
        Set<Class<?>> concreteSubtypes = findAllConcreteSubtypesOf(type);
        Map<Integer, RecordTypeMapper> subTypeMappers = new HashMap<>(concreteSubtypes.size());
        
        for (Class<?> subType : concreteSubtypes) {
            TypeId typeIdAnnotation = subType.getAnnotation(TypeId.class);
            if (typeIdAnnotation == null) {
                throw new InvalidRepresentationElementException("Missing type id on type '" + subType + "'.");
            }
            
            int typeId = typeIdAnnotation.value();
            RecordTypeMapper subTypeMapper;
            if (subType == type) {
                // If the polymorphic type itself is encountered, treat it as a concrete
                // record to avoid endless recursion
                subTypeMapper = (RecordTypeMapper) this.createTypeMapperForConcreteRecord(type);
            } else {
                subTypeMapper = (RecordTypeMapper) this.determineTypeMapperFor(subType);
            }
            
            subTypeMappers.put(typeId, subTypeMapper);
        }
        
        return subTypeMappers;
    }

    private static List<Field> getAllFieldsOf(Class<?> type) {
        List<Field> allFields = new ArrayList<>();
        
        collectFieldsOf(type, allFields::add);
        
        return allFields;
    }
    
    private static void collectFieldsOf(Class<?> type, Consumer<Field> collector) {
        Class<?> superType = type.getSuperclass();
        if (superType != null) {
            collectFieldsOf(superType, collector);
        }
        
        for (Field field : type.getDeclaredFields()) {
            collector.accept(field);
        }
    }
    
    private TypeMapper<?> createTypeMapperForConcreteRecord(Class<?> type) {
        // Create field mappers for the type
        int dataLength = 0;
        List<FieldMapper> fieldMappers = new ArrayList<>();
        for (Field field : getAllFieldsOf(type)) {
            Class<?> fieldType = field.getType();
            Type fieldGenericType = field.getGenericType();
            
            TypeMapper<?> fieldTypeMapper = this.createTypeMapperFor(fieldType, fieldGenericType, field);
            
            String fieldNameCapitalized = Character.toUpperCase(field.getName().charAt(0)) + 
                    field.getName().substring(1);
            String getterName = "get" + fieldNameCapitalized;
            String setterName = "set" + fieldNameCapitalized;
            
            Method getter;
            Method setter;
            try {
                getter = type.getMethod(getterName);
                setter = type.getMethod(setterName, fieldType);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException("Missing accessors for " + field.getName() + " on type " + type + ".", e);
            }
                
            FieldMapper fieldMapper = new FieldMapper(field, getter, setter, fieldTypeMapper);
            dataLength += fieldMapper.getMaxLength();
            fieldMappers.add(fieldMapper);
        }
        
        return new RecordTypeMapper(dataLength, type, fieldMappers);
    }
    
    private TypeMapper<?> createTypeMapperForList(ParameterizedType type, AnnotatedElement element) {        
        MaxLength maxLengthAnnotation = element.getAnnotation(MaxLength.class);
        if (maxLengthAnnotation == null) {
            throw new IllegalArgumentException("MaxLength annotation missing on " + element + ".");
        }
        int maxElements = maxLengthAnnotation.value();
        
        Class<?> elementType = (Class<?>) type.getActualTypeArguments()[0];
        TypeMapper<?> elementMapper = this.determineTypeMapperFor(elementType);
        
        return new ListMapper(maxElements, elementMapper);
    }
    
    /**
     * Determines the size of the given type's representation.
     * @param type The type to determine the size of
     * @return The size of the type's representation in bytes
     */
    public int determineMaxSizeOf(Class<?> type) {
        TypeMapper<?> typeMapper = this.determineTypeMapperFor(type);
        return typeMapper.getMaxLength();
    }
    
    /**
     * Reads an object of the given type from the given data.
     * @param <T> The type to read
     * @param data The data to read from
     * @param type The runtime class of the type to read
     * @return The object read from the data
     */
    @SuppressWarnings("unchecked")
    public <T> T readValue(FixedFormatData data, Class<T> type) {
        TypeMapper<?> typeMapper = this.determineTypeMapperFor(type);
        return (T) typeMapper.readValue(data);
    }
    
    /**
     * Writes an object to the given data object, using the type mapper associated
     * with the runtime type of the object.
     * 
     * @param value The value to write
     * @param data The data object to write to
     */
    public void writeValue(Object value, FixedFormatData data) {
        TypeMapper<?> typeMapper = this.determineTypeMapperFor(value.getClass());
        typeMapper.writeValue(value, data);
    }
    
    /**
     * Writes an object to the given data object, using the type mapper of the
     * given formal type. The formal type needs to be the same type or a supertype of the 
     * object.
     * 
     * @param <T> The formal type of the value
     * @param <P> The actual type of the value
     * @param value The value to write
     * @param formalType The formal type to use for writing the value
     * @param data The data object to write to
     */
    public <T, P extends T> void writeValue(P value, Class<T> formalType, FixedFormatData data) {
        TypeMapper<?> typeMapper = this.determineTypeMapperFor(formalType);
        typeMapper.writeValue(value, data);
    }
    
}
