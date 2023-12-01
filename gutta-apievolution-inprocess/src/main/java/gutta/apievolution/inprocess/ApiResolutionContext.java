package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;

import java.util.Set;

/**
 * The API resolution context provides access to necessary data during the resolution of a consumer API.
 */
class ApiResolutionContext {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final UDTToClassMap typeToClassMap;

    public ApiResolutionContext(ConsumerApiDefinition consumerApiDefinition, RevisionHistory revisionHistory, Set<Integer> supportedRevisions,
            UDTToClassMap typeToClassMap) {

        this.consumerApiDefinition = consumerApiDefinition;
        this.typeToClassMap = typeToClassMap;

        DefinitionResolver definitionResolver = new DefinitionResolver();
        this.definitionResolution = definitionResolver.resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApiDefinition);
    }

    public ConsumerApiDefinition getConsumerApiDefinition() {
        return this.consumerApiDefinition;
    }

    public DefinitionResolution getDefinitionResolution() {
        return this.definitionResolution;
    }

    public UDTToClassMap getTypeToClassMap() {
        return this.typeToClassMap;
    }

}
