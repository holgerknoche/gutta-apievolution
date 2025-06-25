package gutta.apievolution.jmh.inprocess.dynproxy;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;
import gutta.apievolution.jmh.inprocess.CustomerExampleInProcessBenchmarkTemplate;

abstract class CustomerExampleDynProxyBenchmarkTemplate extends CustomerExampleInProcessBenchmarkTemplate {
    
    protected static <T> T createApi(Class<T> apiType, ConsumerApiDefinition consumerApiDefinition) {
        return createApi(apiType, consumerApiDefinition, new DynamicProxyApiMappingStrategy());
    }

}
