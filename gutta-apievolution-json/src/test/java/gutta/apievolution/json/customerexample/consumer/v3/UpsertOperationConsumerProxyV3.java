package gutta.apievolution.json.customerexample.consumer.v3;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.RequestRouter;
import gutta.apievolution.json.consumer.ConsumerOperationProxy;

class UpsertOperationConsumerProxyV3 extends ConsumerOperationProxy<Customer, Customer> {

	public UpsertOperationConsumerProxyV3(ConsumerApiDefinition consumerApi, String apiId, RequestRouter router) {
	    super(consumerApi, apiId, "upsert", "Customer", "Customer", Customer.class, router);
    }
	
}
