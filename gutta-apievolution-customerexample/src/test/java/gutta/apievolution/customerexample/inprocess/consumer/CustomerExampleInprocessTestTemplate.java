package gutta.apievolution.customerexample.inprocess.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.customerexample.CustomerExampleTestTemplate;
import gutta.apievolution.inprocess.ApiMappingStrategy;
import gutta.apievolution.inprocess.ApiResolutionContext;
import gutta.apievolution.inprocess.ApiResolver;
import gutta.apievolution.inprocess.DefaultTypeToClassMap;
import gutta.apievolution.inprocess.UDTToClassMap;

public abstract class CustomerExampleInprocessTestTemplate extends CustomerExampleTestTemplate {
	
	private static final String PROVIDER_PACKAGE_NAME = "gutta.apievolution.customerexample.inprocess.provider";
	
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
