package gutta.apievolution.json;

import java.util.HashMap;
import java.util.Map;

public class TestRequestRouter implements RequestRouter {

    private final Map<String, ProviderOperationProxy> providerProxyMap = new HashMap<>();

    public void registerProviderService(ProviderOperationProxy serviceProxy) {
        this.providerProxyMap.put(serviceProxy.getServiceName(), serviceProxy);
    }

    @Override
    public String invokeService(String consumerApiId, int referencedRevision, String serviceName, String requestJson) {
        ProviderOperationProxy<?, ?> providerProxy = this.providerProxyMap.get(serviceName);
        return providerProxy.invokeService(consumerApiId, referencedRevision, requestJson);
    }

}
