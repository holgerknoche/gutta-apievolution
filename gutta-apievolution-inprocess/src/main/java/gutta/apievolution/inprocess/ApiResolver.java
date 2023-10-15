package gutta.apievolution.inprocess;

import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;

public class ApiResolver {

	private static final ConcurrentMap<Class<?>, Object> API_CACHE = new ConcurrentHashMap<>();
	
	@SuppressWarnings("unchecked")
	public static <T> T resolveApi(ConsumerApiDefinition consumerApiDefinition, Class<T> apiType) {
		return (T) API_CACHE.computeIfAbsent(apiType, type -> new ApiResolver().createApi(consumerApiDefinition, type));
	}
	
	private <T> T createApi(ConsumerApiDefinition consumerApiDefinition, Class<T> apiType) {
		ProviderApiProvider providerApiProvider = this.findProviderApiProvider(consumerApiDefinition.getReferencedApiName());
		// TODO
		return null;
	}
	
	private ProviderApiProvider findProviderApiProvider(String apiName) {
		ServiceLoader<ProviderApiProvider> providerLoader = ServiceLoader.load(ProviderApiProvider.class);		
		
		for (ProviderApiProvider apiProvider : providerLoader) {
			ApiName nameAnnotation = apiProvider.getClass().getAnnotation(ApiName.class);
			if (nameAnnotation == null) {
				continue;
			}
			
			if (apiName.equals(nameAnnotation.value())) {
				return apiProvider;
			}			
		}
		
		throw new NoProviderApiException("Could not find provider API with name '" + apiName + "'.");
	}
	
	static class NoProviderApiException extends RuntimeException {
		
		private static final long serialVersionUID = -5953058597111821886L;

		public NoProviderApiException(String message) {
			super(message);
		}
		
	}
	
}
