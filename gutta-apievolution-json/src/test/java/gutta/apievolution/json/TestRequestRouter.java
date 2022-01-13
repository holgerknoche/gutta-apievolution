package gutta.apievolution.json;

import java.util.HashMap;
import java.util.Map;

public class TestRequestRouter implements RequestRouter {

    private final Map<String, ProviderServiceProxy> providerProxyMap = new HashMap<>();

    public void registerProviderService(ProviderServiceProxy serviceProxy) {
        this.providerProxyMap.put(serviceProxy.getServiceName(), serviceProxy);
    }

    @Override
    public String invokeService(String consumerApiId, int referencedRevision, String serviceName, String requestJson) {
        ProviderServiceProxy<?, ?> providerProxy = this.providerProxyMap.get(serviceName);
        return providerProxy.invokeService(consumerApiId, referencedRevision, requestJson);
    }

}
