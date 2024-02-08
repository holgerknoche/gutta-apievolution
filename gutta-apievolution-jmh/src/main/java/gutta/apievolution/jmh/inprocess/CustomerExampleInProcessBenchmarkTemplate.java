package gutta.apievolution.jmh.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.inprocess.ApiMappingStrategy;
import gutta.apievolution.inprocess.ApiResolutionContext;
import gutta.apievolution.inprocess.ApiResolver;
import gutta.apievolution.inprocess.DefaultTypeToClassMap;
import gutta.apievolution.inprocess.UDTToClassMap;
import gutta.apievolution.jmh.CustomerExampleBenchmarkTemplate;

public abstract class CustomerExampleInProcessBenchmarkTemplate extends CustomerExampleBenchmarkTemplate {

    private static final String PROVIDER_PACKAGE_NAME = "gutta.apievolution.customerexample.inprocess.provider";

    protected static <T> T createApi(Class<T> apiType, ConsumerApiDefinition consumerApiDefinition, ApiMappingStrategy mappingStrategy) {
        String consumerPackageName = apiType.getPackage().getName();

        UDTToClassMap typeToClassMap = new DefaultTypeToClassMap(consumerPackageName, PROVIDER_PACKAGE_NAME);
        ApiResolutionContext resolutionContext = new ApiResolutionContext(consumerApiDefinition, PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS,
                typeToClassMap);
        ApiResolver apiResolver = new ApiResolver(resolutionContext, mappingStrategy);

        return apiResolver.resolveApi(apiType);
    }

}
