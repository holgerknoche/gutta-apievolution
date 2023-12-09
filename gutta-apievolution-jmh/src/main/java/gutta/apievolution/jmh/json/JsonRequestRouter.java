package gutta.apievolution.jmh.json;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.json.ProviderOperationProxy;
import gutta.apievolution.json.RequestRouter;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class JsonRequestRouter implements RequestRouter {
        
    private final DefinitionResolution resolution;
    
    private final Map<String, ProviderOperationProxy<?, ?>> proxyMap;

    public JsonRequestRouter(DefinitionResolution resolution, ProviderOperationProxy<?, ?>... proxies) {
        this.resolution = resolution;
        
        this.proxyMap = Stream.of(proxies)
            .collect(Collectors.toMap(ProviderOperationProxy::getServiceName, Function.identity()));                
    }
    
    @Override
    public String invokeService(String consumerApiId, int referencedRevision, String serviceName, String requestJson) {
        return this.invokeService(serviceName, requestJson);
    }
    
    public String invokeService(String operationName, String requestJson) {
        ProviderOperationProxy<?, ?> proxy = this.proxyMap.get(operationName);
        return proxy.invokeOperation(this.resolution, requestJson);
    }
    
}
