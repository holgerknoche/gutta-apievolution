package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.AbstractApiInvocationHandler;
import gutta.apievolution.inprocess.MethodMappingStrategy;
import gutta.apievolution.inprocess.TypeMappingStrategy;

class DynamicProxyInvocationHandler extends AbstractApiInvocationHandler {

    DynamicProxyInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy, TypeMappingStrategy typeMappingStrategy) {
        super(providerApi, methodMappingStrategy, typeMappingStrategy);
    }

}
