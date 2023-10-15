package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;

public interface ProxyFactory {

	<T> T createProxy(Object providerApi, ConsumerApiDefinition consumerApiDefinition, Class<T> consumerApiType);
	
}
