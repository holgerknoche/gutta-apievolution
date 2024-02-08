package gutta.apievolution.customerexample.fixedformat.consumer;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.customerexample.CustomerExampleTestTemplate;
import gutta.apievolution.customerexample.fixedformat.provider.UpsertOperationProviderProxy;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

public abstract class CustomerExampleFixedFormatTestTemplate extends CustomerExampleTestTemplate {
	
	protected static final Charset CHARSET = StandardCharsets.ISO_8859_1;

	protected <P, R> R invokeProviderMethod(ConsumerApiDefinition consumerApi, ConsumerProxyProvider<P, R> consumerProxyProvider, P parameter) {
        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, consumerApi);
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(definitionResolution, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(definitionResolution, MappingDirection.PROVIDER_TO_CONSUMER);
                
        FixedFormatMapper mapper = new FixedFormatMapper();
        
        UpsertOperationProviderProxy providerProxy = new UpsertOperationProviderProxy(consumerToProviderScript, providerToConsumerScript, mapper, CHARSET);
        RequestRouter requestRouter = new RequestRouter(providerProxy);
        
        ConsumerOperationProxy<P, R> consumerProxy = consumerProxyProvider.createProxy(requestRouter, mapper, CHARSET);
        return consumerProxy.invoke(parameter);
    }
    
    protected interface ConsumerProxyProvider<P, R> {
        
        ConsumerOperationProxy<P, R> createProxy(RequestRouter requestRouter, FixedFormatMapper mapper, Charset charset);
        
    }
	
}
