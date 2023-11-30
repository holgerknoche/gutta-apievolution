package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.InvalidApiException;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Map;

class RecordInvocationHandler implements InvocationHandler {

    private final Object wrappedObject;

    private final Map<Method, FieldMapper> fieldMappers;

    RecordInvocationHandler(Object wrappedObject, Map<Method, FieldMapper> fieldMappers) {
        this.wrappedObject = wrappedObject;
        this.fieldMappers = fieldMappers;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] arguments) {
        FieldMapper fieldMapper = this.fieldMappers.get(method);
        if (fieldMapper == null) {
            throw new InvalidApiException("No field mapper for accessor '" + method + "'.");
        }

        return fieldMapper.mapField(this.wrappedObject);
    }

}
