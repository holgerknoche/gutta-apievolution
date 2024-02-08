package gutta.apievolution.jmh.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;
import gutta.apievolution.jmh.inprocess.CustomerExampleInProcessBenchmarkTemplate;

abstract class CustomerExampleObjectMappingBenchmarkTemplate extends CustomerExampleInProcessBenchmarkTemplate {

    protected static <T> T createApi(Class<T> apiType, ConsumerApiDefinition consumerApiDefinition) {
        return createApi(apiType, consumerApiDefinition, new ObjectMappingApiMappingStrategy());    
    }
    
}
