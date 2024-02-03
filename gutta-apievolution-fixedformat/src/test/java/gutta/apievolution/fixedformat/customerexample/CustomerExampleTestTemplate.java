package gutta.apievolution.fixedformat.customerexample;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.customerexample.provider.UpsertOperationProviderProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public abstract class CustomerExampleTestTemplate {

    protected static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    protected static final ConsumerApiDefinition CONSUMER_API_V1 = ConsumerApiLoader.loadFromClasspath("apis/customerexample/customer-consumer-v1.api", "customer.provider", 0);
    
    protected static final ConsumerApiDefinition CONSUMER_API_V3 = ConsumerApiLoader.loadFromClasspath("apis/customerexample/customer-consumer-v3.api", "customer.provider", 2);
    
    protected static final ConsumerApiDefinition CONSUMER_API_V6 = ConsumerApiLoader.loadFromClasspath("apis/customerexample/customer-consumer-v6.api", "customer.provider", 5);
    
    protected static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/customerexample/customer-provider-revision-1.api", "apis/customerexample/customer-provider-revision-2.api",
            "apis/customerexample/customer-provider-revision-3.api", "apis/customerexample/customer-provider-revision-4.api",
            "apis/customerexample/customer-provider-revision-5.api", "apis/customerexample/customer-provider-revision-6.api");
    
    protected static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1, 2, 3, 4, 5));
 
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
