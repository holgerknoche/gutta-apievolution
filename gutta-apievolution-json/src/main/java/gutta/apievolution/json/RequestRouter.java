package gutta.apievolution.json;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;

public interface RequestRouter {

    String invokeService(String consumerApiId, int referencedRevision, String serviceName, String requestJson);

}
