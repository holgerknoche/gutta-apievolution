package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * An {@link ApiResolver} locates provider APIs and creates objects for accessing provider APIs from a consumer.
 */
public class ApiResolver {

    private final ApiResolutionContext apiResolutionContext;

    private final ApiMappingStrategy apiMappingStrategy;

    private final ConcurrentMap<Class<?>, Object> apiCache = new ConcurrentHashMap<>();

    /**
     * Creates a new API resolver using the given data.
     * 
     * @param resolutionContext  The resolution context to use
     * @param apiMappingStrategy The API mapping strategy to use
     */
    public ApiResolver(ApiResolutionContext resolutionContext, ApiMappingStrategy apiMappingStrategy) {
        this.apiResolutionContext = resolutionContext;
        this.apiMappingStrategy = apiMappingStrategy;
    }

    /**
     * Resolves the API corresponding to the given consumer API type and returns an object for accessing this API.
     * 
     * @param <T>     The type of the consumer API
     * @param apiType The desired consumer API type
     * @return An object for accessing the API
     */
    @SuppressWarnings("unchecked")
    public <T> T resolveApi(Class<T> apiType) {
        return (T) this.apiCache.computeIfAbsent(apiType, type -> this.createApi(type));
    }

    private <T> T createApi(Class<T> apiType) {
        // Locate API provider for the provider API object and attempt to retrieve the
        // API object for the requested version
        ConsumerApiDefinition consumerApiDefinition = this.apiResolutionContext.getConsumerApiDefinition();
        Object providerApi = this.createProviderApi(consumerApiDefinition.getReferencedApiName(), consumerApiDefinition.getReferencedRevision());

        // Create a proxy to adapt the provider API to the client API
        DefinitionResolution definitionResolution = this.apiResolutionContext.getDefinitionResolution();
        UDTToClassMap typeToClassMap = this.apiResolutionContext.getTypeToClassMap();
        return this.apiMappingStrategy.mapApi(providerApi, consumerApiDefinition, definitionResolution, typeToClassMap, apiType);
    }

    private Object createProviderApi(String providerApiName, int providerApiRevision) {
        ProviderApiProvider providerApiProvider = this.findProviderApiProvider(providerApiName);
        Object providerApi = providerApiProvider.createApi(providerApiRevision);

        if (providerApi == null) {
            throw new NoProviderApiException(
                    "Provider API provider for API '" + providerApiName + "' returned no valid API object for revision " + providerApiRevision + ".");
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
