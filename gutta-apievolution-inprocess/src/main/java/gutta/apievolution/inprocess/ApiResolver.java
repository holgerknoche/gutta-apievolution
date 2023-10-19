package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class ApiResolver {

    private final ApiResolutionContext apiResolutionContext;

    private final ProxyFactory proxyFactory;

    private final ConcurrentMap<Class<?>, Object> apiCache = new ConcurrentHashMap<>();

    public ApiResolver(ApiResolutionContext resolutionContext) {
        this(resolutionContext, new DynamicProxyFactory());
    }

    public ApiResolver(ApiResolutionContext resolutionContext, ProxyFactory proxyFactory) {
        this.apiResolutionContext = resolutionContext;
        this.proxyFactory = proxyFactory;
    }

    @SuppressWarnings("unchecked")
    public <T> T resolveApi(Class<T> apiType) {
        return (T) this.apiCache.computeIfAbsent(apiType, type -> this.createApi(type));
    }

    private <T> T createApi(Class<T> apiType) {
        // Locate API provider for the provider API object and attempt to retrieve the
        // API object for the requested version
        ConsumerApiDefinition consumerApiDefinition = this.apiResolutionContext.getConsumerApiDefinition();
        Object providerApi = this.createProviderApi(consumerApiDefinition.getReferencedApiName(),
                consumerApiDefinition.getReferencedRevision());

        // Create a proxy to adapt the provider API to the client API
        DefinitionResolution definitionResolution = this.apiResolutionContext.getDefinitionResolution();
        TypeToClassMap typeToClassMap = this.apiResolutionContext.getTypeToClassMap();
        return this.proxyFactory.createProxy(providerApi, consumerApiDefinition, definitionResolution, typeToClassMap, apiType);
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
