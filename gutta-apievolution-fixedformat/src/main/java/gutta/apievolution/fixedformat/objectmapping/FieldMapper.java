package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

class FieldMapper {

    private final Method readAccessor;
    
    private final Method writeAccessor;
    
    private final TypeMapper typeMapper;
    
    public FieldMapper(Method readAccessor, Method writeAccessor, TypeMapper typeMapper) {
        this.readAccessor = readAccessor;
        this.writeAccessor = writeAccessor;
        this.typeMapper = typeMapper;
    }
    
    public int getMaxLength() {
        return this.typeMapper.getMaxLength();
    }
    
    public void writeValue(Object object, FixedFormatData data) {
        try {
            Object fieldValue = this.readAccessor.invoke(object);
            this.typeMapper.writeValue(fieldValue, data);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

}
