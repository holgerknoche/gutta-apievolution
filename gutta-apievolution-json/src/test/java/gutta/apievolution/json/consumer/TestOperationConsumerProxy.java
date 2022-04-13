package gutta.apievolution.json.consumer;

import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

public class TestOperationConsumerProxy
        extends ConsumerOperationProxy {

    private static final String API_ID = "apis/consumer-api.api";

    private static final int REFERENCED_REVISION = 0;

    private static final String PARAMETER_TYPE_NAME = "ConsumerParameter";

    private static final String RESULT_TYPE_NAME = "ConsumerResult";

    private static final String OPERATION_NAME = "testOperation";

    public TestOperationConsumerProxy(RequestRouter router) {
        super(ConsumerApiLoader.loadFromClasspath(API_ID, REFERENCED_REVISION), PARAMETER_TYPE_NAME, RESULT_TYPE_NAME,
                router);
    }

    public ConsumerResult invokeProviderMethod(ConsumerParameter parameter) {
        return this.invokeMethod(API_ID, REFERENCED_REVISION, OPERATION_NAME, parameter, ConsumerResult.class);
    }

}
