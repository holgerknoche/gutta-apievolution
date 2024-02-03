package gutta.apievolution.customerexample;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;

public abstract class CustomerExampleTestTemplate {
	
    protected static final ConsumerApiDefinition CONSUMER_API_V1 = ConsumerApiLoader.loadFromClasspath("apis/customer-consumer-v1.api", "customer.provider", 0);
    
    protected static final ConsumerApiDefinition CONSUMER_API_V3 = ConsumerApiLoader.loadFromClasspath("apis/customer-consumer-v3.api", "customer.provider", 2);
    
    protected static final ConsumerApiDefinition CONSUMER_API_V6 = ConsumerApiLoader.loadFromClasspath("apis/customer-consumer-v6.api", "customer.provider", 5);
    
    protected static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/customer-provider-revision-1.api", "apis/customer-provider-revision-2.api",
            "apis/customer-provider-revision-3.api", "apis/customer-provider-revision-4.api",
            "apis/customer-provider-revision-5.api", "apis/customer-provider-revision-6.api");
    
    protected static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1, 2, 3, 4, 5));

}
