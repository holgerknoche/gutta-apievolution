package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.ValueMapper;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;

class RecordTypeValueMapper implements ValueMapper {

    private final Class<?> targetInterface;

    private final Map<Method, FieldMapper> fieldMappers;

    public RecordTypeValueMapper(Class<?> targetInterface, Map<Method, FieldMapper> fieldMappers) {
        this.targetInterface = targetInterface;
        this.fieldMappers = fieldMappers;
    }

    @Override
    public Object mapValue(Object value) {
        InvocationHandler invocationHandler = new RecordInvocationHandler(value, fieldMappers);
        return Proxy.newProxyInstance(this.getClass().getClassLoader(), new Class<?>[] { this.targetInterface }, invocationHandler);
    }

}
