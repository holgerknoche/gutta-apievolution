package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod500ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult500> {

    public TestMethod500ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod500", "ConsumerResult500", ConsumerResult500.class, router);
    }
    
}
