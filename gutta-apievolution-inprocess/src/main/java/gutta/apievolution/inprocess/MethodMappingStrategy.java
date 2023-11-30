package gutta.apievolution.inprocess;

import java.lang.reflect.Method;

/**
 * A {@link MethodMappingStrategy} encapsulates the specifics of mapping invocations to API methods. They employ {@link ApiMethodInvoker} objects that handle
 * the technicalities of invoking a particular method, which are expected to be stateless and therefore cacheable for a given API object.
 */
public interface MethodMappingStrategy {

    /**
     * Creates a new method invoker for the given method on the given API type.
     * 
     * @param apiType             The type of the API object (i.e., the provider implementation of the API)
     * @param method              The method to be invoked on the API object
     * @param typeMappingStrategy The type mapping strategy to use for the invocation
     * @return The created method invoker
     */
    ApiMethodInvoker createMethodInvoker(Class<?> apiType, Method method, TypeMappingStrategy typeMappingStrategy);

}
