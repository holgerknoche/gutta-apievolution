package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod25ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult25> {

    public TestMethod25ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethod25", "ConsumerResult25", ConsumerResult25.class, router);
    }
    
}
