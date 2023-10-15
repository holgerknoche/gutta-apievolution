package gutta.apievolution.inprocess;

public interface ProxyFactory {

    <T> T createProxy(Object providerApi, ResolvedConsumerApiDefinition consumerApiDefinition, Class<T> consumerApiType);

}
