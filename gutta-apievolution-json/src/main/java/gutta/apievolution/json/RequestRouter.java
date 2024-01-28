package gutta.apievolution.json;

/**
 * Interface for request routers, i.e., objects that route consumer requests to
 * provider operations.
 */
public interface RequestRouter {

    /**
     * Invokes an operation given by its name using the given data.
     * 
     * @param consumerApiId      The consumer API used for the request
     * @param referencedRevision The provider revision referenced by the consumer
     *                           API
     * @param operationName      The name of the operation to invoke
     * @param requestJson        The request in JSON format (conforming to the
     *                           consumer API definition)
     * @return The result in JSON format (conforming to the consumer API definition)
     */
    byte[] invokeOperation(String consumerApiId, int referencedRevision, String operationName, byte[] requestJson);

}
