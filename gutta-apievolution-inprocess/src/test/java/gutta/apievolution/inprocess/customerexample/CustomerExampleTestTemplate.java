package gutta.apievolution.inprocess.customerexample;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.inprocess.ApiMappingStrategy;
import gutta.apievolution.inprocess.ApiResolutionContext;
import gutta.apievolution.inprocess.ApiResolver;
import gutta.apievolution.inprocess.DefaultTypeToClassMap;
import gutta.apievolution.inprocess.UDTToClassMap;
import gutta.apievolution.inprocess.dynproxy.DynamicProxyApiMappingStrategy;
import gutta.apievolution.inprocess.objectmapping.ObjectMappingApiMappingStrategy;

public abstract class CustomerExampleTestTemplate {

	protected static final ConsumerApiDefinition CONSUMER_API_V1 = ConsumerApiLoader
			.loadFromClasspath("apis/customerexample/customer-consumer-v1.api", "customer.provider", 0);

	protected static final ConsumerApiDefinition CONSUMER_API_V3 = ConsumerApiLoader
			.loadFromClasspath("apis/customerexample/customer-consumer-v3.api", "customer.provider", 2);

	protected static final ConsumerApiDefinition CONSUMER_API_V6 = ConsumerApiLoader
			.loadFromClasspath("apis/customerexample/customer-consumer-v6.api", "customer.provider", 5);

	protected static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
			"apis/customerexample/customer-provider-revision-1.api",
			"apis/customerexample/customer-provider-revision-2.api",
			"apis/customerexample/customer-provider-revision-3.api",
			"apis/customerexample/customer-provider-revision-4.api",
			"apis/customerexample/customer-provider-revision-5.api",
			"apis/customerexample/customer-provider-revision-6.api");

	protected static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1, 2, 3, 4, 5));

	private static final String PROVIDER_PACKAGE_NAME = "gutta.apievolution.inprocess.customerexample.provider";

	protected static Stream<ApiMappingStrategy> strategies() {
		return Stream.of(new ObjectMappingApiMappingStrategy(), new DynamicProxyApiMappingStrategy());
	}
	
	protected <T> T createApi(Class<T> apiType, Package consumerPackage, ConsumerApiDefinition consumerApiDefinition,
			ApiMappingStrategy mappingStrategy) {
		String consumerPackageName = consumerPackage.getName();

		UDTToClassMap typeToClassMap = new DefaultTypeToClassMap(consumerPackageName, PROVIDER_PACKAGE_NAME);
		ApiResolutionContext resolutionContext = new ApiResolutionContext(consumerApiDefinition,
				PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, typeToClassMap);
		ApiResolver apiResolver = new ApiResolver(resolutionContext, mappingStrategy);
		
		return apiResolver.resolveApi(apiType);
	}

}
