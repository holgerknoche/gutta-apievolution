package gutta.apievolution.json;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SimpleJsonRequestRouter implements RequestRouter {

	private final Map<String, ProviderOperationProxy<?, ?>> operationNameToProxyMap;
	
	public SimpleJsonRequestRouter(ProviderOperationProxy<?, ?>... proxies) {
		this.operationNameToProxyMap = Stream.of(proxies)
			.collect(Collectors.toMap(ProviderOperationProxy::getOperationName, Function.identity()));
	}
	
	@Override
	public byte[] invokeOperation(String consumerApiId, int referencedRevision, String operationName, byte[] requestJson) {
		ProviderOperationProxy<?, ?> proxy = this.operationNameToProxyMap.get(operationName);
		if (proxy == null) {
			throw new IllegalArgumentException("Unsupported operation '" + operationName + "'.");
		}
		 
		return proxy.invokeOperation(consumerApiId, operationName, referencedRevision, requestJson);
	}

}
