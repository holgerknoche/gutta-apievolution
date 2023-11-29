package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.AbstractApiInvocationHandler;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeMappingStrategy;
import gutta.apievolution.inprocess.ValueMapper;

class DynamicProxyInvocationHandler extends AbstractApiInvocationHandler {

    DynamicProxyInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy, TypeMappingStrategy typeMappingStrategy) {
        super(providerApi, methodMappingStrategy, typeMappingStrategy);
    }
    
    @Override
    protected Object handleExceptionOnApiInvocation(Exception exception) {
        ValueMapper valueMapper = this.typeMappingStrategy.mapperFor(exception.getClass());
        if (valueMapper == null) {
            return this.handleUnmappedException(exception);
        }

        Object mappedExceptionData = valueMapper.mapValue(exception);
        throw new MappedException(mappedExceptionData);
    }
    
    private Object handleUnmappedException(Exception exception) {
        if (exception instanceof RuntimeException) {
            throw (RuntimeException) exception;
        } else {
            throw new UnmappedCheckedException(exception);
        }
    }
    
    static class UnmappedCheckedException extends RuntimeException {
        
        private static final long serialVersionUID = 1956489966858044004L;

        public UnmappedCheckedException(Throwable cause) {
            super("Unmapped checked exception thrown in API invocation.", cause);
        }
        
    }

}
