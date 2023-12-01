package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.inprocess.AbstractApiInvocationHandler;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeMappingStrategy;
import gutta.apievolution.inprocess.UnmappedException;
import gutta.apievolution.inprocess.ValueMapper;

class ObjectMappingInvocationHandler extends AbstractApiInvocationHandler {

    ObjectMappingInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy, TypeMappingStrategy typeMappingStrategy) {
        super(providerApi, methodMappingStrategy, typeMappingStrategy);
    }

    @Override
    protected Object handleExceptionOnApiInvocation(Exception exception) throws Exception {
        ValueMapper exceptionMapper = this.typeMappingStrategy.mapperFor(exception.getClass());
        if (exceptionMapper == null) {
            throw new UnmappedException(exception);
        }

        Exception mappedException = (Exception) exceptionMapper.mapValue(exception);
        throw mappedException;
    }

}
