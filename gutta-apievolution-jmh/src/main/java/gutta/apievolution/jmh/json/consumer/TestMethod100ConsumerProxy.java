package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethod100ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult100> {

    public TestMethod100ConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "ConsumerResult100", router);
    }

    @Override
    protected Class<ConsumerResult100> getResultClass() {
        return ConsumerResult100.class;
    }

    @Override
    protected String getOperationName() {
        return "testMethod100";
    }
    
}
