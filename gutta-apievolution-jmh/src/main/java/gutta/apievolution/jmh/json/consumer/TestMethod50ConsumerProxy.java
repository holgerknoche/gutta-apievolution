package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod50ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult50> {

    public TestMethod50ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "ConsumerResult50", router);
    }

    @Override
    protected String getOperationName() {
        return "testMethod50";
    }
    
}
