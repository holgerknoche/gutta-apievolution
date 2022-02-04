package gutta.apievolution.repository;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolutionException;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.APIParseException;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;

/**
 * Service for accessing and managing consumer API definitions.
 */
@ApplicationScoped
public class ConsumerApisService {

    @Inject
    ProviderApisService providerApisService;

    @Inject
    ConsumerApisRepository apisRepository;

    /**
     * Saves the given consumer API definition provided that it is consistent.
     * @param referencedHistory The history name of the referenced provider definition
     * @param referencedRevisionNumber The revision number of the referenced provider definition
     * @param consumerName The name of the consumer
     * @param apiDefinition The API definition to save
     */
    @Transactional
    public void saveConsumerApi(String referencedHistory, int referencedRevisionNumber, String consumerName,
                                String apiDefinition) {
        // Parse the given API definition
        ConsumerApiDefinition consumerDefinition;
        try {
            consumerDefinition = ConsumerApiLoader.loadFromString(apiDefinition, referencedRevisionNumber);
        } catch (APIParseException e) {
            throw new ApiProcessingException("Error processing API definition: " + e.getMessage(), e);
        }

        // Try to load the referenced provider API definition
        PersistentProviderApiDefinition referencedDefinition =
                this.providerApisService.readApiRevision(referencedHistory, referencedRevisionNumber)
                        .orElseThrow(() -> new ApiProcessingException("The referenced revision does not exist."));

        ProviderApiDefinition referencedRevision = ProviderApiLoader.loadFromString(
                referencedDefinition.getRevisionNumber(),
                referencedDefinition.getDefinitionText(),
                true,
                Optional.empty());

        // Attempt to resolve the two revisions against each other
        try {
            RevisionHistory revisionHistory = new RevisionHistory(referencedRevision);
            // TODO Actually load the supported revisions and deny APIs referencing unsupported definitions
            Set<Integer> supportedRevisions = Collections.singleton(referencedRevision.getRevision());
            new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerDefinition);
        } catch (DefinitionResolutionException e) {
            throw new ApiProcessingException("The definition is incompatible to the referenced provider definition: " +
                    e.getMessage(), e);
        }

        // If no errors occur, the consumer definition can be saved
        PersistentConsumerApiDefinition persistentConsumerDefinition = new PersistentConsumerApiDefinition();
        persistentConsumerDefinition.setConsumerName(consumerName);
        persistentConsumerDefinition.setReferencedRevision(referencedDefinition);
        persistentConsumerDefinition.setDefinitionText(apiDefinition);

        this.apisRepository.saveConsumerApiDefinition(persistentConsumerDefinition);
    }

}
