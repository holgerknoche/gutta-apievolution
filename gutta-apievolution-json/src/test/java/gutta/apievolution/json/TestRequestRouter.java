package gutta.apievolution.json;

import java.util.HashMap;
import java.util.Map;

public class TestRequestRouter implements RequestRouter {

	private final String apiName;
	
    private final Map<String, ProviderOperationProxy<?, ?>> providerOperationProxyMap = new HashMap<>();

    TestRequestRouter(String apiName) {
    	this.apiName = apiName;
    }
    
    public void registerProviderService(ProviderOperationProxy<?, ?> operationProxy) {
        this.providerOperationProxyMap.put(operationProxy.getServiceName(), operationProxy);
    }

    @Override
    public String invokeService(String consumerApiId, int referencedRevision, String operationName, String requestJson) {
        ProviderOperationProxy<?, ?> operationProxy = this.providerOperationProxyMap.get(operationName);
        return operationProxy.invokeOperation(consumerApiId, this.apiName, referencedRevision, requestJson);
    }

}
