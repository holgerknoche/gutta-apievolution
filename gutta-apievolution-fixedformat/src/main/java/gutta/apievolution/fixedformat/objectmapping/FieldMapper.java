package gutta.apievolution.fixedformat.objectmapping;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import static gutta.apievolution.fixedformat.objectmapping.Flags.*;

class FieldMapper {
    
    private static final int FLAGS_SIZE = 1;

    private final Field field;
    
    private final MethodHandle readAccessor;

    private final MethodHandle writeAccessor;

    private final TypeMapper<?> typeMapper;

    public FieldMapper(Field field, Method readAccessor, Method writeAccessor, TypeMapper<?> typeMapper) {
        this.field = field;
        
        Lookup lookup = MethodHandles.publicLookup();

        try {
            this.readAccessor = lookup.unreflect(readAccessor);
            this.writeAccessor = lookup.unreflect(writeAccessor);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }

        this.typeMapper = typeMapper;
    }

    public int getMaxLength() {
        return (this.typeMapper.getMaxLength() + FLAGS_SIZE);
    }

    public void readValue(FixedFormatData data, Object target) {
        try {
            byte flags = data.readFlagsByte();
            Object value = this.readValue(flags, data);
                        
            this.writeAccessor.invoke(target, value);
        } catch (Throwable e) {
            throw new RuntimeException("Error reading value for field '" + this.field + "'.", e);
        }
    }
    
    private Object readValue(byte flags, FixedFormatData data) {
        TypeMapper<?> mapper = this.typeMapper;
        
        switch (flags) {
        case IS_ABSENT:            
            data.skipBytes(mapper.getMaxLength());
            return null;
            
        case IS_PRESENT:
            return mapper.readValue(data);
            
        case IS_UNREPRESENTABLE:
            data.skipBytes(mapper.getMaxLength());
            return mapper.handleUnrepresentableValue();
            
        default:
            throw new InvalidDataException("Unsupported flags value '" + flags + "'.");
        }
    }

    public void writeValue(Object object, FixedFormatData data) {
        try {
            Object fieldValue = this.readAccessor.invoke(object);

            TypeMapper<?> mapper = this.typeMapper;
            if (fieldValue == null) {
                // If no value is present, write the appropriate flag and
                // fill the data with padding
                data.writeFlagsByte(IS_ABSENT);                
                data.writePadding(mapper.getMaxLength());                
            } else if (mapper.isUnrepresentable(fieldValue)) {
                // If a value is present but unrepresentable, set the appropriate
                // flag and fill the data with padding
                data.writeFlagsByte(IS_UNREPRESENTABLE);
                data.writePadding(mapper.getMaxLength());
            } else {
                // A representable value is written as-is
                data.writeFlagsByte(IS_PRESENT);
                mapper.writeValue(fieldValue, data);
            }
        } catch (Throwable e) {
            throw new RuntimeException("Error writing value for field '" + this.field + "'.", e);
        }
    }

}
