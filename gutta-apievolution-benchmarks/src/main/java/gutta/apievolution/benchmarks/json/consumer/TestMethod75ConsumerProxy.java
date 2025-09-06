package gutta.apievolution.benchmarks.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod75ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult75> {

    public TestMethod75ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod75", "ConsumerResult75", ConsumerResult75.class, router);
    }
    
}
