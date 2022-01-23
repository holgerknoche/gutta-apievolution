package gutta.apievolution.json.consumer;

import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.json.ConsumerInvocationProxy;
import gutta.apievolution.json.RequestRouter;

public class TestProviderProxy extends ConsumerInvocationProxy {

    private static final String API_ID = "apis/consumer-api.api";

    private static final int REFERENCED_REVISION = 0;

    private static final String PARAMETER_TYPE_NAME = "ConsumerParameter";

    private static final String RESULT_TYPE_NAME = "ConsumerResult";

    private static final String SERVICE_NAME = "TestService";

    public TestProviderProxy(RequestRouter router) {
        super(ConsumerApiLoader.loadFromClasspath(API_ID, REFERENCED_REVISION),
                PARAMETER_TYPE_NAME, RESULT_TYPE_NAME, router);
    }

    public ConsumerResult invokeProviderMethod(ConsumerParameter parameter) {
        return this.invokeMethod(API_ID, REFERENCED_REVISION, SERVICE_NAME, parameter, ConsumerResult.class);
    }

}
