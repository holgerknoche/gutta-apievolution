package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public abstract class AbstractApiInvocationHandler implements InvocationHandler {

    private final Object providerApi;

    private final MethodMappingStrategy methodMappingStrategy;

    protected final TypeMappingStrategy typeMappingStrategy;

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

        try {
            return methodInvoker.invokeApiMethod(this.providerApi, parameterObject);
        } catch (Exception e) {
            return this.handleExceptionOnApiInvocation(e);
        }
    }

    protected Object getProviderApi() {
        return this.providerApi;
    }
    
    protected abstract Object handleExceptionOnApiInvocation(Exception exception);

}
