package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;

import java.nio.ByteBuffer;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * A {@link RequestRouter} routes requests from consumer proxies to the appropriate provider proxies.
 */
public class RequestRouter {

    private final Map<String, ProviderOperationProxy<?, ?>> proxyLookup;

    /**
     * Creates a new router that routes to the given proxies.
     * 
     * @param proxies The proxies to register with this router
     */
    public RequestRouter(ProviderOperationProxy<?, ?>... proxies) {
        this.proxyLookup = Stream.of(proxies).collect(Collectors.toMap(ProviderOperationProxy::getOperationName, Function.identity()));
    }

    /**
     * Routes the request with the given data to the appropriate provider proxy.
     * 
     * @param operationName The name of the invoked operation
     * @param parameterData The parameter data to pass to the proxy
     * @param resultData    The buffer to store the result data in
     * @return The result buffer
     */
    public ByteBuffer routeRequest(String operationName, ByteBuffer parameterData, ByteBuffer resultData) {
        ProviderOperationProxy<?, ?> proxy = this.proxyLookup.get(operationName);
        if (proxy == null) {
            throw new IllegalArgumentException("No proxy for operation name '" + operationName + "'.");
        }

        proxy.invoke(parameterData, resultData);
        return resultData;
    }

}
