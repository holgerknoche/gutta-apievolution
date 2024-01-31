package gutta.apievolution.json.customerexample;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;
import gutta.apievolution.json.SimpleJsonRequestRouter;
import gutta.apievolution.json.customerexample.provider.UpsertOperationProviderProxy;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public abstract class CustomerExampleTestTemplate {
    
    protected static final String CONSUMER_API_ID_V1 = "apis/customerexample/customer-consumer-v1.api";
    
    protected static final ConsumerApiDefinition CONSUMER_API_V1 = ConsumerApiLoader.loadFromClasspath(CONSUMER_API_ID_V1, "customer.provider", 0);
    
    protected static final String CONSUMER_API_ID_V3 = "apis/customerexample/customer-consumer-v3.api";
    
    protected static final ConsumerApiDefinition CONSUMER_API_V3 = ConsumerApiLoader.loadFromClasspath(CONSUMER_API_ID_V3, "customer.provider", 2);
    
    protected static final String CONSUMER_API_ID_V6 = "apis/customerexample/customer-consumer-v6.api";
    
    protected static final ConsumerApiDefinition CONSUMER_API_V6 = ConsumerApiLoader.loadFromClasspath(CONSUMER_API_ID_V6, "customer.provider", 5);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/customerexample/customer-provider-revision-1.api", "apis/customerexample/customer-provider-revision-2.api",
            "apis/customerexample/customer-provider-revision-3.api", "apis/customerexample/customer-provider-revision-4.api",
            "apis/customerexample/customer-provider-revision-5.api", "apis/customerexample/customer-provider-revision-6.api");
    
    private static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1, 2, 3, 4, 5));
 
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
