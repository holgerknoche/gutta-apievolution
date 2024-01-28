package gutta.apievolution.json.unrepresentablevalues;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;

class UnrepresentableValuesTest {

    private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath("apis/unrepresentablevalues/consumer-api.api",
            "test.provider", 0);

    private static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/unrepresentablevalues/provider-revision-1.api", "apis/unrepresentablevalues/provider-revision-2.api"
            );

    private static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1));
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, CONSUMER_API);
    
    @Test
    void unrepresentableSubtype() {
    	// TODO
    }
	
}
