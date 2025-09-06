package gutta.apievolution.benchmarks.inprocess.objectmapping;

import gutta.apievolution.benchmarks.inprocess.CustomerExampleInProcessBenchmarkTemplate;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;

abstract class CustomerExampleObjectMappingBenchmarkTemplate extends CustomerExampleInProcessBenchmarkTemplate {

    protected static <T> T createApi(Class<T> apiType, ConsumerApiDefinition consumerApiDefinition) {
        return createApi(apiType, consumerApiDefinition, new ObjectMappingApiMappingStrategy());    
    }
    
}
