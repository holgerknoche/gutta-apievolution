package gutta.apievolution.jmh.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.inprocess.ApiMappingStrategy;
import gutta.apievolution.inprocess.ApiResolutionContext;
import gutta.apievolution.inprocess.ApiResolver;
import gutta.apievolution.inprocess.DefaultTypeToClassMap;

import java.util.Collections;
import java.util.Set;

public abstract class InProcessConversionBenchmarkTemplate {

    private static final ConsumerApiDefinition CONSUMER_API_DEFINITION = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-api.api");

    private static final Set<Integer> SUPPORTED_REVISIONS = Collections.singleton(0);
    
    protected static <T> T createConsumerApi(Class<T> apiType, ApiMappingStrategy apiMappingStrategy, String consumerPackageName) {
        // Create an API resolution context and an API resolver
           ApiResolutionContext resolutionContext = new ApiResolutionContext(CONSUMER_API_DEFINITION, PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS,
                   new DefaultTypeToClassMap(consumerPackageName, "gutta.apievolution.jmh.inprocess.provider"));
           ApiResolver apiResolver = new ApiResolver(resolutionContext, apiMappingStrategy);

           // Resolve the API
           return apiResolver.resolveApi(apiType);
       }
    
}
