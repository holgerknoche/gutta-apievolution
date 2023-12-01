package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Specific implementation of a method invoker that uses reflection (based on {@link Method} invocations) for invoking API methods.
 */
public class ReflectiveMethodInvoker extends AbstractApiMethodInvoker {

    private final Method apiMethod;

    /**
     * Creates a new invoker using the given data.
     * 
     * @param typeMappingStrategy The type mapping strategy to use
     * @param apiMethod           The API method to invoke
     */
    public ReflectiveMethodInvoker(TypeMappingStrategy typeMappingStrategy, Method apiMethod) {
        super(typeMappingStrategy);

        this.apiMethod = apiMethod;
    }

    @Override
    public Object invokeMethod(Object apiObject, Object parameterObject) throws Exception {
        try {
            return this.apiMethod.invoke(apiObject, parameterObject);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();

            if (cause instanceof Exception) {
                throw (Exception) cause;
            } else {
                throw new InvalidInvocationException("Error invoking API method.", e);
            }
        } catch (IllegalAccessException | IllegalArgumentException e) {
            throw new InvalidInvocationException("Error invoking API method.", e);
        }
    }

}
