package gutta.apievolution.inprocess;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;

class ReflectiveFieldMapper implements FieldMapper {
    
    private final MethodHandle fieldAccessorHandle;

    private final ValueMapper valueMapper;

    protected ReflectiveFieldMapper(Method fieldAccessor, ValueMapper valueMapper) {
        this.fieldAccessorHandle = findHandleFor(fieldAccessor);
        this.valueMapper = valueMapper;
    }

    private static MethodHandle findHandleFor(Method accessor) {
        try {
            return MethodHandles.publicLookup().unreflect(accessor);
        } catch (IllegalAccessException e) {
            throw new InvalidApiException("Could not look up method handle for accessor '" + accessor + "',", e);
        }
    }
    
    @Override
    public final Object mapField(Object targetObject) {
        Object value = this.determineFieldValue(targetObject);

        return this.valueMapper.mapValue(value);
    }

    private Object determineFieldValue(Object targetObject) {
        try {
            return this.fieldAccessorHandle.invoke(targetObject);
        } catch (Throwable e) {
            throw new InvalidInvocationException("Error invoking field accessor.", e);
        }
    }

}
