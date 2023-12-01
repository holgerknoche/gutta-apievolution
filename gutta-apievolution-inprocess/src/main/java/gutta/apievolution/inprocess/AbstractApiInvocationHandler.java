package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/**
 * Abstract supertype for API invocation handlers that provides common functionality. Such invocation handlers are used for adapting API implementations to
 * consumer expectations by using dynamic proxies.
 */
public abstract class AbstractApiInvocationHandler implements InvocationHandler {

    private final Object providerApi;

    private final MethodMappingStrategy methodMappingStrategy;

    protected final TypeMappingStrategy typeMappingStrategy;

    /**
     * Creates a new invocation handler using the given data.
     * 
     * @param providerApi           The provider API object that provides access to the concrete implementation
     * @param methodMappingStrategy The method mapping strategy to use
     * @param typeMappingStrategy   The type mapping strategy to use
     */
    protected AbstractApiInvocationHandler(Object providerApi, MethodMappingStrategy methodMappingStrategy, TypeMappingStrategy typeMappingStrategy) {
        this.providerApi = providerApi;
        this.methodMappingStrategy = methodMappingStrategy;
        this.typeMappingStrategy = typeMappingStrategy;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] arguments) throws Exception {
        if (arguments.length != 1) {
            throw new InvalidInvocationException(
                    "Method '" + method.getName() + "' was invoked with invalid number of arguments (expected 1, but got " + arguments.length + ").");
        }

        Object parameterObject = arguments[0];
        ApiMethodInvoker methodInvoker = this.methodMappingStrategy.createMethodInvoker(this.providerApi.getClass(), method, this.typeMappingStrategy);

        try {
            return methodInvoker.invokeApiMethod(this.providerApi, parameterObject);
        } catch (Exception e) {
            return this.handleExceptionOnApiInvocation(e);
        }
    }

    /**
     * Returns the provider API object that is adapted to the consumer perspective.
     * 
     * @return see above
     */
    protected Object getProviderApi() {
        return this.providerApi;
    }

    /**
     * Encapsulates the behavior when an exception occurs during an API invocation.
     * 
     * @param exception The exception that occured during an API invocation
     * @return An object that is to be returned to the invoker (i.e., the consumer)
     * @throws Exception If an exception is thrown in response
     */
    protected abstract Object handleExceptionOnApiInvocation(Exception exception) throws Exception;

}
