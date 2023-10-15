package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;

import java.util.Set;

public class ResolvedConsumerApiDefinition {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    ResolvedConsumerApiDefinition(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution) {
        this.consumerApiDefinition = consumerApiDefinition;
        this.definitionResolution = definitionResolution;
    }

    public static ResolvedConsumerApiDefinition create(ConsumerApiDefinition consumerApiDefinition, RevisionHistory revisionHistory,
            Set<Integer> supportedRevisions) {

        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions,
                consumerApiDefinition);
        return new ResolvedConsumerApiDefinition(consumerApiDefinition, definitionResolution);
    }

    ConsumerApiDefinition getConsumerApiDefinition() {
        return this.consumerApiDefinition;
    }
    
    DefinitionResolution getDefinitionResolution() {
        return this.definitionResolution;
    }
    
    public String getReferencedApiName() {
        return this.getConsumerApiDefinition().getReferencedApiName();
    }
    
    public int getReferencedRevision() {
        return this.getConsumerApiDefinition().getReferencedRevision();
    }
        
}
