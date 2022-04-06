package gutta.apievolution.json;

/**
 * Interface for request routers, i.e., objects that route consumer requests to
 * provider services.
 */
public interface RequestRouter {

    /**
     * Invokes a service given by its name using the given data.
     * 
     * @param consumerApiId      The consumer API used for the request
     * @param referencedRevision The provider revision referenced by the consumer
     *                           API
     * @param serviceName        The name of the service to invoke
     * @param requestJson        The request in JSON format (conforming to the
     *                           consumer API definition)
     * @return The result in JSON format (conforming to the consumer API definition)
     */
    String invokeService(String consumerApiId, int referencedRevision, String serviceName, String requestJson);

}
