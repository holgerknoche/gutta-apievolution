package gutta.apievolution.benchmarks.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod250ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult250> {

    public TestMethod250ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod250", "ConsumerResult250", ConsumerResult250.class, router);
    }
    
}
