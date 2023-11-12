package gutta.apievolution.inprocess;

import java.lang.reflect.Method;

public interface MethodMappingStrategy {
    
    ApiMethodInvoker createMethodInvoker(Class<?> apiType, Method method, TypeMappingStrategy typeMappingStrategy);

}
