package gutta.apievolution.benchmarks.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;

public class TestMethodEmptyConsumerProxy extends ConsumerProxyTemplate<EmptyConsumerResult> {

    public TestMethodEmptyConsumerProxy(ConsumerApiDefinition apiDefinition, RequestRouter router) {
        super(apiDefinition, "testMethodEmpty", "EmptyConsumerResult", EmptyConsumerResult.class, router);
    }

}
