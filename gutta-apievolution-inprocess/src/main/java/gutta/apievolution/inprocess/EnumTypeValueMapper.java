package gutta.apievolution.inprocess;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.function.Supplier;

class EnumTypeValueMapper implements ValueMapper {
    
    private final Map<Enum<?>, Enum<?>> memberMap;
    
    private final Supplier<?> onUnrepresentableValue;

    public EnumTypeValueMapper(Class<?> targetType, Map<Enum<?>, Enum<?>> memberMap) {        
        this.memberMap = memberMap;
        this.onUnrepresentableValue = determineActionOnUnrepresentableValue(targetType);
    }
    
    private static Supplier<?> determineActionOnUnrepresentableValue(Class<?> targetType) {        
        // First, look for a value annotated with the appropriate annotation
        Enum<?> unrepresentableValue = findAnnotatedUnrepresentableValue(targetType);
        if (unrepresentableValue != null) {
            return () -> unrepresentableValue;
        }
        
        // If no annotated value is found, look for an annotated method
        UnrepresentableValueSupplier supplier = UnrepresentableValueSupplier.findSupplierOnType(targetType);
        if (supplier != null) {
            return supplier;
        }        
        
        // If no behavior is specified, use the default behavior
        return Defaults::onUnrepresentableValue;
    }
    
    private static Enum<?> findAnnotatedUnrepresentableValue(Class<?> targetType) {
        for (Enum<?> value : valuesOfEnum(targetType)) {
            try {
                String memberName = value.name();                                
                Field field = targetType.getField(memberName);
                
                if (field.isAnnotationPresent(UnrepresentableValue.class)) {
                    return value;
                }
            } catch (NoSuchFieldException e) {
                throw new InvalidApiException("Unable to determine annotations for value '" + value + "'.", e);
            }
        }
        
        return null;
    }
    
    private static Enum<?>[] valuesOfEnum(Class<?> enumClass) {
        try {
            Method method = enumClass.getMethod("values");
            return (Enum<?>[]) method.invoke(null);
        } catch (NoSuchMethodException | SecurityException | IllegalAccessException | InvocationTargetException e) {
            throw new InvalidApiException("Unable to determine the values of enum type '" + enumClass + "'.", e);
        }
    }
    
    @Override
    public boolean isRepresentable(Object value) {
        return this.memberMap.containsKey(value);
    }
    
    @Override
    public Object mapValue(Object value) {
        if (value == null) {
            return null;
        }
        
        Object mappedValue = this.memberMap.get(value);
        if (mappedValue != null) {
            return mappedValue;
        } else {
            return this.onUnrepresentableValue.get();
        }
    }

}
