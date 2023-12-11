package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod250ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult250> {

    public TestMethod250ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "ConsumerResult250", router);
    }

    @Override
    protected String getOperationName() {
        return "testMethod250";
    }
    
}
