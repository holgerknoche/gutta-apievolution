package gutta.apievolution.jmh.inprocess;

import gutta.apievolution.inprocess.ApiMappingStrategy;
import gutta.apievolution.inprocess.ApiResolutionContext;
import gutta.apievolution.inprocess.ApiResolver;
import gutta.apievolution.inprocess.DefaultTypeToClassMap;
import gutta.apievolution.jmh.JMHBenchmarkTemplate;

public abstract class InProcessConversionBenchmarkTemplate extends JMHBenchmarkTemplate {
    
    protected static <T> T createConsumerApi(Class<T> apiType, ApiMappingStrategy apiMappingStrategy, String consumerPackageName) {
        // Create an API resolution context and an API resolver
           ApiResolutionContext resolutionContext = new ApiResolutionContext(CONSUMER_API_DEFINITION, PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS,
                   new DefaultTypeToClassMap(consumerPackageName, "gutta.apievolution.jmh.inprocess.provider"));
           ApiResolver apiResolver = new ApiResolver(resolutionContext, apiMappingStrategy);

           // Resolve the API
           return apiResolver.resolveApi(apiType);
       }
    
}
