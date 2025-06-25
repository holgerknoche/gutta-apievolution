package gutta.apievolution.jmh;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;

import java.util.Collections;
import java.util.Set;

public abstract class JMHBenchmarkTemplate {

    protected static final ConsumerApiDefinition CONSUMER_API_DEFINITION = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);

    protected static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-api.api");

    protected static final Set<Integer> SUPPORTED_REVISIONS = Collections.singleton(0);

    protected static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY,
            SUPPORTED_REVISIONS, CONSUMER_API_DEFINITION);

}
