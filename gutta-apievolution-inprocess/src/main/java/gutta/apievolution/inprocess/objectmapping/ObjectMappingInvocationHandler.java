package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.inprocess.AbstractApiInvocationHandler;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeMappingStrategy;

class ObjectMappingInvocationHandler extends AbstractApiInvocationHandler {

    ObjectMappingInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy, TypeMappingStrategy typeMappingStrategy) {
        super(providerApi, methodMappingStrategy, typeMappingStrategy);
    }

    @Override
    protected Object handleExceptionOnApiInvocation(Exception exception) {
        // TODO Auto-generated method stub
        return null;
    }

}
