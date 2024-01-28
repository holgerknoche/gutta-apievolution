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
        this.providerOperationProxyMap.put(operationProxy.getOperationName(), operationProxy);
    }

    @Override
    public byte[] invokeOperation(String consumerApiId, int referencedRevision, String operationName, byte[] requestJson) {
        ProviderOperationProxy<?, ?> operationProxy = this.providerOperationProxyMap.get(operationName);
        return operationProxy.invokeOperation(consumerApiId, this.apiName, referencedRevision, requestJson);
    }

}
