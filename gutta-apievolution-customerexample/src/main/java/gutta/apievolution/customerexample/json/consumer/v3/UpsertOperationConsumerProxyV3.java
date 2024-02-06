package gutta.apievolution.customerexample.json.consumer.v3;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

public class UpsertOperationConsumerProxyV3 extends ConsumerOperationProxy<Customer, Customer> {

	public UpsertOperationConsumerProxyV3(ConsumerApiDefinition consumerApi, String apiId, RequestRouter router) {
	    super(consumerApi, apiId, "upsert", "Customer", "Customer", Customer.class, router);
    }
	
}
