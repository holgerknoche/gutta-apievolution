package gutta.apievolution.fixedformat.objectmapping;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.reflect.Method;

class FieldMapper {

    private final MethodHandle readAccessor;

    private final MethodHandle writeAccessor;

    private final TypeMapper<?> typeMapper;

    public FieldMapper(Method readAccessor, Method writeAccessor, TypeMapper<?> typeMapper) {
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
        return this.typeMapper.getMaxLength();
    }

    public void readValue(FixedFormatData data, Object target) {
        try {
            Object value = this.typeMapper.readValue(data);
            this.writeAccessor.invoke(target, value);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    public void writeValue(Object object, FixedFormatData data) {
        try {
            Object fieldValue = this.readAccessor.invoke(object);
            this.typeMapper.writeValue(fieldValue, data);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

}
