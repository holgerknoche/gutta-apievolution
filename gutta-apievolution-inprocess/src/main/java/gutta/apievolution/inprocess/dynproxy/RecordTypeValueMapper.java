package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.AbstractRecordTypeValueMapper;
import gutta.apievolution.inprocess.FieldMapper;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;

class RecordTypeValueMapper extends AbstractRecordTypeValueMapper {

    private final Class<?> targetInterface;

    private final Map<Method, FieldMapper> fieldMappers;

    public RecordTypeValueMapper(Class<?> targetInterface, Map<Method, FieldMapper> fieldMappers) {
        super(targetInterface);
        
        this.targetInterface = targetInterface;
        this.fieldMappers = fieldMappers;
    }

    @Override
    public Object mapRepresentableValue(Object value) {
        InvocationHandler invocationHandler = new RecordInvocationHandler(value, this.fieldMappers);
        return Proxy.newProxyInstance(this.getClass().getClassLoader(), new Class<?>[] { this.targetInterface }, invocationHandler);
    }

}
