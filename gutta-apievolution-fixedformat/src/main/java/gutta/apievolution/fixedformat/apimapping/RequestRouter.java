package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;

import java.nio.ByteBuffer;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RequestRouter {
    
    private final Map<String, ProviderOperationProxy<?, ?>> proxyLookup;
    
    public RequestRouter(ProviderOperationProxy<?, ?>... proxies) {
        this.proxyLookup = Stream.of(proxies)
                .collect(Collectors.toMap(ProviderOperationProxy::getOperationName, Function.identity()));
    }
    
    public ByteBuffer routeRequest(String operationName, ByteBuffer parameterData, ByteBuffer resultData) {
        ProviderOperationProxy<?, ?> proxy = this.proxyLookup.get(operationName);
        if (proxy == null) {
            throw new IllegalArgumentException("No proxy for operation name '" + operationName + "'.");
        }
        
        proxy.invoke(parameterData, resultData);        
        return resultData;
    }

}
