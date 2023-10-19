package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;

import java.util.Set;

public class ApiResolutionContext {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final TypeToClassMap typeToClassMap;
    
    public ApiResolutionContext(ConsumerApiDefinition consumerApiDefinition, RevisionHistory revisionHistory,
            Set<Integer> supportedRevisions, TypeToClassMap typeToClassMap) {

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
    
    public TypeToClassMap getTypeToClassMap() {
        return this.typeToClassMap;
    }

}
