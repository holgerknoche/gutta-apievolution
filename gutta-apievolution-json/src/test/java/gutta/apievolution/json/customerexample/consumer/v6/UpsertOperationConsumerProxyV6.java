package gutta.apievolution.json.customerexample.consumer.v6;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

class UpsertOperationConsumerProxyV6 extends ConsumerOperationProxy<Customer, Customer> {

	public UpsertOperationConsumerProxyV6(ConsumerApiDefinition consumerApi, String apiId, RequestRouter router) {
	    super(consumerApi, apiId, "upsert", "Customer", "Customer", Customer.class, router);
    }
	
}
