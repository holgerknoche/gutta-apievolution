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
import org.junit.jupiter.api.Test;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Test cases based on the customer example from the paper. 
 */
class CustomerExampleTest {
    
    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    private static final ConsumerApiDefinition CONSUMER_API_V1 = ConsumerApiLoader.loadFromClasspath("apis/customerexample/customer-consumer-v1.api", "customer.provider", 0);
    
    private static final ConsumerApiDefinition CONSUMER_API_V3 = ConsumerApiLoader.loadFromClasspath("apis/customerexample/customer-consumer-v3.api", "customer.provider", 2);
    
    private static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/customerexample/customer-provider-revision-1.api", "apis/customerexample/customer-provider-revision-2.api",
            "apis/customerexample/customer-provider-revision-3.api", "apis/customerexample/customer-provider-revision-4.api",
            "apis/customerexample/customer-provider-revision-5.api", "apis/customerexample/customer-provider-revision-6.api");
    
    private static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1, 2, 3, 4, 5));
    
    @Test
    void invokeV6ProviderFromV1Consumer() {
        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, CONSUMER_API_V1);
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(definitionResolution, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(definitionResolution, MappingDirection.PROVIDER_TO_CONSUMER);
    }
    
    @Test
    void invokeV6ProviderFromV3Consumer() {
        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, CONSUMER_API_V3);
    }       

}
