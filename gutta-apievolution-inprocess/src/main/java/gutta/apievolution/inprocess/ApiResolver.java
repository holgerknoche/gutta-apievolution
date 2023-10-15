package gutta.apievolution.inprocess;

import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class ApiResolver {

    private static final ConcurrentMap<Class<?>, Object> API_CACHE = new ConcurrentHashMap<>();

    private static final ApiResolver RESOLVER_INSTANCE = configureResolverInstance();

    private final ProxyFactory proxyFactory;

    ApiResolver(ProxyFactory proxyFactory) {
        this.proxyFactory = proxyFactory;
    }

    private static ApiResolver configureResolverInstance() {
        return new ApiResolver(new DynamicProxyFactory());
    }

    @SuppressWarnings("unchecked")
    public static <T> T resolveApi(ResolvedConsumerApiDefinition consumerApiDefinition, Class<T> apiType) {
        return (T) API_CACHE.computeIfAbsent(apiType, type -> RESOLVER_INSTANCE.createApi(consumerApiDefinition, type));
    }

    private <T> T createApi(ResolvedConsumerApiDefinition consumerApiDefinition, Class<T> apiType) {
        // Locate API provider for the provider API object and attempt to retrieve the
        // API object for the requested version
        Object providerApi = this.createProviderApi(consumerApiDefinition.getReferencedApiName(),
                consumerApiDefinition.getReferencedRevision());

        // Create a proxy to adapt the provider API to the client API
        return this.proxyFactory.createProxy(providerApi, consumerApiDefinition, apiType);
    }

    private Object createProviderApi(String providerApiName, int providerApiRevision) {
        ProviderApiProvider providerApiProvider = this.findProviderApiProvider(providerApiName);
        Object providerApi = providerApiProvider.createApi(providerApiRevision);

        if (providerApi == null) {
            throw new NoProviderApiException("Provider API provider for API '" + providerApiName +
                    "' returned no valid API object for revision " + providerApiRevision + ".");
        }

        return providerApi;
    }

    private ProviderApiProvider findProviderApiProvider(String apiName) {
        ServiceLoader<ProviderApiProvider> providerLoader = ServiceLoader.load(ProviderApiProvider.class);

        for (ProviderApiProvider apiProvider : providerLoader) {
            ApiName nameAnnotation = apiProvider.getClass().getAnnotation(ApiName.class);
            if (nameAnnotation == null) {
                continue;
            }

            if (apiName.equals(nameAnnotation.value())) {
                return apiProvider;
            }
        }

        throw new NoProviderApiException("Could not find provider API with name '" + apiName + "'.");
    }

    static class NoProviderApiException extends RuntimeException {

        private static final long serialVersionUID = -5953058597111821886L;

        public NoProviderApiException(String message) {
            super(message);
        }

    }

}
