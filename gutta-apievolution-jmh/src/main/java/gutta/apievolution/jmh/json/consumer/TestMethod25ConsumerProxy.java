package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod25ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult25> {

    public TestMethod25ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "ConsumerResult25", router);
    }

    @Override
    protected String getOperationName() {
        return "testMethod25";
    }
    
}
