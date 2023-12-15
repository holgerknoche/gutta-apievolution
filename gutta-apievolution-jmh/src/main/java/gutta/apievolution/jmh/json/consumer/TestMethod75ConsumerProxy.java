package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod75ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult75> {

    public TestMethod75ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "ConsumerResult75", router);
    }

    @Override
    protected String getOperationName() {
        return "testMethod75";
    }
    
}
