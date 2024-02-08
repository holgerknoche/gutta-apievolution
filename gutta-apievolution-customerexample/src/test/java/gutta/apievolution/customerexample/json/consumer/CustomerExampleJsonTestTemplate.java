package gutta.apievolution.customerexample.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.customerexample.CustomerExampleTestTemplate;
import gutta.apievolution.customerexample.json.provider.UpsertOperationProviderProxy;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;
import gutta.apievolution.json.SimpleJsonRequestRouter;

public abstract class CustomerExampleJsonTestTemplate extends CustomerExampleTestTemplate {
	
	protected static final String CONSUMER_API_ID_V1 = "apis/customer-consumer-v1.api";
	
	protected static final String CONSUMER_API_ID_V3 = "apis/customer-consumer-v3.api";
	
	protected static final String CONSUMER_API_ID_V6 = "apis/customer-consumer-v6.api";
	
	protected <P, R> R invokeProviderMethod(ConsumerApiDefinition consumerApi, String apiId, ConsumerProxyProvider<P, R> consumerProxyProvider, P parameter) {
        UpsertOperationProviderProxy providerProxy = new UpsertOperationProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS);
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerOperationProxy<P, R> consumerProxy = consumerProxyProvider.createProxy(consumerApi, apiId, requestRouter);
        return consumerProxy.invokeOperation(parameter);
    }
    
    protected interface ConsumerProxyProvider<P, R> {
        
        ConsumerOperationProxy<P, R> createProxy(ConsumerApiDefinition consumerApi, String apiId, RequestRouter router);
        
    }

}
