package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class ReflectiveMethodInvoker extends AbstractApiMethodInvoker {

    private final Method apiMethod;

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
