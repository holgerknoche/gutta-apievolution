package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

abstract class InProcessMappingTestTemplate<S extends ApiMappingStrategy> {

    protected <T> T loadAndResolveApi(Class<T> apiType, String consumerPackageName) {
        // Load the consumer and provider API definitions
        ConsumerApiDefinition consumerApiDefinition = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);

        RevisionHistory providerRevisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api");
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));

        // Create an API resolution context and an API resolver
        ApiResolutionContext resolutionContext = new ApiResolutionContext(consumerApiDefinition, providerRevisionHistory, supportedRevisions,
                new DefaultTypeToClassMap(consumerPackageName, "gutta.apievolution.inprocess.provider"));
        ApiResolver apiResolver = new ApiResolver(resolutionContext, this.apiMappingStrategy());

        // Resolve the API
        return apiResolver.resolveApi(apiType);
    }

    protected abstract S apiMappingStrategy();

}
