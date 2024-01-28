package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod50ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult50> {

    public TestMethod50ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod50", "ConsumerResult50", ConsumerResult50.class, router);
    }
    
}
