package gutta.apievolution.json.consumer;

import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

public class TestOperationConsumerProxy extends ConsumerOperationProxy<ConsumerParameter, ConsumerResult> {

	private static final String API_ID = "apis/consumer-api.api";

	private static final String REFERENCED_API_NAME = "test.provider";

	private static final int REFERENCED_REVISION = 0;

	private static final String PARAMETER_TYPE_NAME = "ConsumerParameter";

	private static final String RESULT_TYPE_NAME = "ConsumerResult";

	private static final String OPERATION_NAME = "testOperation";

	public TestOperationConsumerProxy(RequestRouter router) {
		super(ConsumerApiLoader.loadFromClasspath(API_ID, REFERENCED_API_NAME, REFERENCED_REVISION),
				API_ID, OPERATION_NAME, PARAMETER_TYPE_NAME, RESULT_TYPE_NAME, ConsumerResult.class, router);
	}

}
