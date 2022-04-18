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

public class FixedFormatMapper {
    
    private final ConcurrentMap<Class<?>, TypeMapper> typeMappers = new ConcurrentHashMap<>();
    
    private synchronized TypeMapper determineTypeMapperFor(Class<?> type, AnnotatedElement element) {
        TypeMapper mapper = this.typeMappers.get(type);
        if (mapper != null) {
            return mapper;
        }
        
        return this.createTypeMapperFor(type, null, element);
    }
    
    private TypeMapper createTypeMapperFor(Class<?> type, Type genericType, AnnotatedElement element) {
        TypeMapper typeMapper;
        
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

    private TypeMapper createTypeMapperForRecord(Class<?> type) {
        int maxLength = 0;
        List<FieldMapper> fieldMappers = new ArrayList<>();
        for (Field field : type.getDeclaredFields()) {
            Class<?> fieldType = field.getType();
            Type fieldGenericType = field.getGenericType();
            
            TypeMapper fieldTypeMapper = this.createTypeMapperFor(fieldType, fieldGenericType, field);
            
            String fieldNameCapitalized = Character.toUpperCase(field.getName().charAt(0)) + field.getName().substring(1);
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
        
        return new RecordTypeMapper(maxLength, fieldMappers);
    }
    
    private TypeMapper createTypeMapperForList(ParameterizedType type, AnnotatedElement element) {        
        MaxLength maxLengthAnnotation = element.getAnnotation(MaxLength.class);
        if (maxLengthAnnotation == null) {
            throw new IllegalArgumentException("MaxLength annotation missing on " + element + ".");
        }
        int maxElements = maxLengthAnnotation.value();
        
        Class<?> elementType = (Class<?>) type.getActualTypeArguments()[0];
        TypeMapper elementMapper = this.determineTypeMapperFor(elementType, null);
        
        return new ListMapper(maxElements, elementMapper);
    }
    
    public int determineMaxSizeOf(Class<?> type) {
        TypeMapper typeMapper = this.determineTypeMapperFor(type, null);
        return typeMapper.getMaxLength();
    }
    
    public <T> T readValue(FixedFormatData data, Class<T> type) {
        // TODO
        return null;
    }
    
    public void writeValue(Object value, FixedFormatData data) {
        TypeMapper typeMapper = this.determineTypeMapperFor(value.getClass(), null);
        typeMapper.writeValue(value, data);
    }
    
}
