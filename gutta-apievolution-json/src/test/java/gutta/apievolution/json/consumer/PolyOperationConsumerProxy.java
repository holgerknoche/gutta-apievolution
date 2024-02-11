package gutta.apievolution.json.consumer;

import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

public class PolyOperationConsumerProxy extends ConsumerOperationProxy<ConsumerSuperType, ConsumerSuperType> {
    
    private static final String API_ID = "apis/consumer-api.api";

    private static final String REFERENCED_API_NAME = "test.provider";

    private static final int REFERENCED_REVISION = 0;

    private static final String PARAMETER_TYPE_NAME = "ConsumerSuperType";

    private static final String RESULT_TYPE_NAME = "ConsumerSuperType";

    private static final String OPERATION_NAME = "polyOperation";
    
    public PolyOperationConsumerProxy(RequestRouter requestRouter) {
        super(ConsumerApiLoader.loadFromClasspath(API_ID, REFERENCED_API_NAME, REFERENCED_REVISION),
                API_ID, OPERATION_NAME, PARAMETER_TYPE_NAME, RESULT_TYPE_NAME, ConsumerSuperType.class, requestRouter);
    }

}
