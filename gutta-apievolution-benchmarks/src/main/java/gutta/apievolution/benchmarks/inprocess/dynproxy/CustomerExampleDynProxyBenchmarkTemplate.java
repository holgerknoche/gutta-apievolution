package gutta.apievolution.benchmarks.inprocess.dynproxy;

import gutta.apievolution.benchmarks.inprocess.CustomerExampleInProcessBenchmarkTemplate;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;

abstract class CustomerExampleDynProxyBenchmarkTemplate extends CustomerExampleInProcessBenchmarkTemplate {
    
    protected static <T> T createApi(Class<T> apiType, ConsumerApiDefinition consumerApiDefinition) {
        return createApi(apiType, consumerApiDefinition, new DynamicProxyApiMappingStrategy());
    }

}
