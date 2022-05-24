package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

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
            int fieldLength = element.getAnnotation(MaxLength.class).value();
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
        int maxLength = 0;
        List<FieldMapper> fieldMappers = new ArrayList<>();
        for (Field field : type.getDeclaredFields()) {
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
                
            FieldMapper fieldMapper = new FieldMapper(getter, setter, fieldTypeMapper);
            maxLength += fieldMapper.getMaxLength();
            fieldMappers.add(fieldMapper);
        }
        
        return new RecordTypeMapper(maxLength, type, fieldMappers);
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
     * Writes an object to the given data object.
     * @param value The value to write
     * @param data The data object to write to
     */
    public void writeValue(Object value, FixedFormatData data) {
        TypeMapper<?> typeMapper = this.determineTypeMapperFor(value.getClass());
        typeMapper.writeValue(value, data);
    }
    
}
