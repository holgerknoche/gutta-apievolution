package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public abstract class AbstractApiInvocationHandler implements InvocationHandler {

    private final Object providerApi;

    private final MethodMappingStrategy methodMappingStrategy;

    private final TypeMappingStrategy typeMappingStrategy;

    protected AbstractApiInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy,
            TypeMappingStrategy typeMappingStrategy) {
        this.providerApi = providerApi;
        this.methodMappingStrategy = methodMappingStrategy;
        this.typeMappingStrategy = typeMappingStrategy;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] arguments) {
        if (arguments.length != 1) {
            throw new InvalidInvocationException("Method '" + method.getName() +
                    "' was invoked with invalid number of arguments (expected 1, but got " + arguments.length + ").");
        }

        Object parameterObject = arguments[0];
        ApiMethodInvoker methodInvoker = this.methodMappingStrategy.createMethodInvoker(this.providerApi.getClass(), method,
                this.typeMappingStrategy);

        return methodInvoker.invokeApiMethod(this.providerApi, parameterObject);
    }

    protected Object getProviderApi() {
        return this.providerApi;
    }

}
