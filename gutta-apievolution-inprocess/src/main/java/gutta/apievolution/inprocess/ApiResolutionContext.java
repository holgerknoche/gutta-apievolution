package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;

import java.util.Set;

/**
 * The API resolution context provides access to necessary data during the resolution of a consumer API.
 */
public class ApiResolutionContext {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final UDTToClassMap typeToClassMap;

    /**
     * Creates a new resolution context with the given data.
     * 
     * @param consumerApiDefinition The underlying consumer API definition
     * @param revisionHistory       The provider API revision history to use
     * @param supportedRevisions    The set of supported revisions
     * @param typeToClassMap        The map of the user-defined types to their representing classes
     */
    public ApiResolutionContext(ConsumerApiDefinition consumerApiDefinition, RevisionHistory revisionHistory, Set<Integer> supportedRevisions,
            UDTToClassMap typeToClassMap) {

        this.consumerApiDefinition = consumerApiDefinition;
        this.typeToClassMap = typeToClassMap;

        DefinitionResolver definitionResolver = new DefinitionResolver();
        this.definitionResolution = definitionResolver.resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApiDefinition);
    }

    /**
     * Returns the underlying consumer API definition.
     * 
     * @return see above
     */
    public ConsumerApiDefinition getConsumerApiDefinition() {
        return this.consumerApiDefinition;
    }

    /**
     * Returns the definition resolution of the consumer API against the provider API revision history.
     * 
     * @return see above
     */
    public DefinitionResolution getDefinitionResolution() {
        return this.definitionResolution;
    }

    /**
     * Returns the map of API types to their representing classes.
     * 
     * @return see above
     */
    public UDTToClassMap getTypeToClassMap() {
        return this.typeToClassMap;
    }

}
