package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.AbstractApiInvocationHandler;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeMappingStrategy;
import gutta.apievolution.inprocess.UnmappedException;
import gutta.apievolution.inprocess.ValueMapper;

class DynamicProxyInvocationHandler extends AbstractApiInvocationHandler {

    DynamicProxyInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy, TypeMappingStrategy typeMappingStrategy) {
        super(providerApi, methodMappingStrategy, typeMappingStrategy);
    }

    @Override
    protected Object handleExceptionOnApiInvocation(Exception exception) {
        ValueMapper valueMapper = this.typeMappingStrategy.mapperFor(exception.getClass());
        if (valueMapper == null) {
            throw new UnmappedException(exception);
        }

        Object mappedExceptionData = valueMapper.mapValue(exception);
        throw new MappedException(mappedExceptionData);
    }

}
