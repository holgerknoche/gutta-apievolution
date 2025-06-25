package gutta.apievolution.benchmarks.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod100ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult100> {

    public TestMethod100ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod100", "ConsumerResult100", ConsumerResult100.class, router);
    }
    
}
