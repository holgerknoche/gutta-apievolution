package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod10ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult10> {

    public TestMethod10ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "ConsumerResult10", router);
    }

    @Override
    protected String getOperationName() {
        return "testMethod10";
    }
    
}
