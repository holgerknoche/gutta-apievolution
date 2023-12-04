package gutta.apievolution.inprocess;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.function.Supplier;

class UnrepresentableValueSupplier implements Supplier<Object> {

    private final Method underlyingMethod;
    
    static UnrepresentableValueSupplier findSupplierOnType(Class<?> type) {
        for (Method method : type.getMethods()) {
            if (method.isAnnotationPresent(UnrepresentableValue.class)) {
                return createSupplierFor(method);
            }
        }

        return null;
    }
    
    private static UnrepresentableValueSupplier createSupplierFor(Method method) {
        if (!Modifier.isStatic(method.getModifiers())) {
            throw new InvalidApiException("Method '" + method + "' is annotated as an unrepresentable value provider, but is not static.");
        }

        return new UnrepresentableValueSupplier(method);
    }
    
    private UnrepresentableValueSupplier(Method underlyingMethod) {
        this.underlyingMethod = underlyingMethod;
    }
    
    @Override
    public Object get() {
        try {
            return this.underlyingMethod.invoke(null);
        } catch (IllegalAccessException | IllegalArgumentException e) {
            throw new InvalidApiException("Unable to access the unrepresentable value supplier '" + this.underlyingMethod + "'.", e);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            
            if (cause instanceof RuntimeException) {
                // Runtime exceptions thrown from the underlying supplier are passed as-is, otherwise all custom exceptions would be
                // wrapped
                throw (RuntimeException) cause;
            } else {            
                throw new InvalidInvocationException("Error invoking the unrepresentable value supplier '" + this.underlyingMethod + "'.", e);
            }
        }
    }

}
