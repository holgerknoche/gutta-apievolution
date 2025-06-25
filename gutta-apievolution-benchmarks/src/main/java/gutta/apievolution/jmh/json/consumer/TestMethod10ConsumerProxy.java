package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod10ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult10> {

    public TestMethod10ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod10", "ConsumerResult10", ConsumerResult10.class, router);
    }
    
}
