package gutta.apievolution.inprocess;

/**
 * An {@link ApiMethodInvoker} encapsulates the implementation for invoking a particular method on a provider API.
 */
public interface ApiMethodInvoker {

    /**
     * Invokes the method represented by this invoker on the given API object.
     * 
     * @param apiObject       The API object to invoke the method on
     * @param parameterObject The parameter object (adapted to the provider's view) that is passed to the method
     * @return The result object returned by the method
     * @throws Exception If the invoked method throws an exception
     */
    Object invokeApiMethod(Object apiObject, Object parameterObject) throws Exception;

}
