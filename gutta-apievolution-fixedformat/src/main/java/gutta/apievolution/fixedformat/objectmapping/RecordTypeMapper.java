package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.function.Supplier;

class RecordTypeMapper implements TypeMapper<Object> {
            
    private final int dataLength;
    
    private final Class<?> recordType;
    
    private final Supplier<Object> instanceSupplier;
    
    public final List<FieldMapper> fieldMappers;
  
    public RecordTypeMapper(int dataLength, Class<?> recordType, List<FieldMapper> fieldMappers) {        
        this.dataLength = dataLength;
        this.recordType = recordType;
        this.instanceSupplier = createInstanceSupplier(recordType);
        this.fieldMappers = fieldMappers;
    }
    
    private static Supplier<Object> createInstanceSupplier(Class<?> type) {
        try {
            Constructor<?> constructor = type.getConstructor();
            return new ConstructorBasedInstanceSupplier(constructor);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }
    
    @Override
    public boolean isCacheable() {
        return true;
    }
        
    public int getMaxLength() {
        return this.dataLength;
    }
        
    @Override
    public Object readValue(FixedFormatData data) {
        Object instance = this.instanceSupplier.get();        
        this.fieldMappers.forEach(mapper -> mapper.readValue(data, instance));       
        return instance;
    }
    
    @Override
    public Object handleUnrepresentableValue() {
        // TODO Auto-generated method stub
        return null;
    }
                
    @Override
    public void writeValue(Object value, FixedFormatData data) {        
        this.fieldMappers.forEach(mapper -> mapper.writeValue(value, data));
    }
    
    private static class ConstructorBasedInstanceSupplier implements Supplier<Object> {

        private final Constructor<?> constructor;
        
        public ConstructorBasedInstanceSupplier(Constructor<?> constructor) {
            this.constructor = constructor;
        }
        
        @Override
        public Object get() {            
            try {
                return constructor.newInstance();
            } catch (InstantiationException | InvocationTargetException | IllegalAccessException e) {
                throw new RuntimeException(e);
            }
        }
        
    }
    
    @Override
    public String toString() {
        return "Record type mapper for " + this.recordType;
    }
    
}
