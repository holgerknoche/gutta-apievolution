package gutta.apievolution.inprocess.dynproxy;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

abstract class ReflectiveFieldMapper implements FieldMapper {

    private final Method fieldAccessor;

    protected ReflectiveFieldMapper(Method fieldAccessor) {
        this.fieldAccessor = fieldAccessor;
    }

    @Override
    public final Object mapField(Object targetObject) {
        Object value = this.determineFieldValue(targetObject);

        return this.mapValue(value);
    }

    private Object determineFieldValue(Object targetObject) {
        try {
            return this.fieldAccessor.invoke(targetObject);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new InvalidInvocationException("Error invoking field accessor.", e);
        }
    }

    protected abstract Object mapValue(Object value);

}
