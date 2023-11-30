package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

class ReflectiveFieldMapper implements FieldMapper {

    private final Method fieldAccessor;

    private final ValueMapper valueMapper;

    protected ReflectiveFieldMapper(Method fieldAccessor, ValueMapper valueMapper) {
        this.fieldAccessor = fieldAccessor;
        this.valueMapper = valueMapper;
    }

    @Override
    public final Object mapField(Object targetObject) {
        Object value = this.determineFieldValue(targetObject);

        return this.valueMapper.mapValue(value);
    }

    private Object determineFieldValue(Object targetObject) {
        try {
            return this.fieldAccessor.invoke(targetObject);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new InvalidInvocationException("Error invoking field accessor.", e);
        }
    }

}
