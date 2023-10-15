package gutta.apievolution.repository;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolutionException;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.APIParseException;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

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
     * Reads a consumer API definition by its ID.
     * 
     * @param id The ID of the definition
     * @return The definition, if it exists
     */
    public Optional<PersistentConsumerApiDefinition> readConsumerApi(Integer id) {
        return this.apisRepository.findById(id);
    }

    /**
     * Creates a mapping based on the given consumer API definition for the desired
     * format and mode.
     * 
     * @param id     The id of the consumer API definition to use
     * @param format The format for which the mapping should be created
     * @param type   The type of mapping to be created
     * @return The mapping in the appropriate representation
     */
    public MappingRepresentation mapConsumerApi(Integer id, String format, ApiMappingType type) {
        Optional<PersistentConsumerApiDefinition> optionalConsumerApi = this.readConsumerApi(id);
        if (optionalConsumerApi.isEmpty()) {
            throw new ApiProcessingException("The desired consumer API does not exist.");
        }

        PersistentConsumerApiDefinition persistentConsumerApi = optionalConsumerApi.get();
        PersistentProviderApiDefinition persistentProviderApi = persistentConsumerApi.getReferencedRevision();

        ConsumerApiDefinition consumerApiDefinition = ConsumerApiLoader.loadFromString(persistentConsumerApi.getDefinitionText(),
                persistentProviderApi.getHistoryName(), persistentProviderApi.getRevisionNumber());

        String historyName = persistentProviderApi.getHistoryName();
        RevisionHistory revisionHistory = this.providerApisService.readRevisionHistory(historyName);
        Set<Integer> supportedRevisions = this.providerApisService.readSupportedRevisions(historyName);

        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions,
                consumerApiDefinition);

        ApiMappingRepresentationCreator mappingCreator = this.getMappingCreatorFor(format)
                .orElseThrow(() -> new ApiProcessingException("No mapping creator for format " + format + "."));

        switch (type) {
        case CONSUMER:
            return mappingCreator.createConsumerSideMapping(definitionResolution);

        case PROVIDER:
            return mappingCreator.createProviderSideMapping(definitionResolution);

        case FULL:
            return mappingCreator.createFullMapping(definitionResolution);

        default:
            throw new ApiProcessingException("Unsupported mapping type " + type + ".");
        }
    }

    private Optional<ApiMappingRepresentationCreator> getMappingCreatorFor(String format) {
        if (format == null) {
            return Optional.empty();
        }

        switch (format) {
        case "json":
            return Optional.of(new JsonMappingRepresentationCreator());

        case "mappingscript":
            return Optional.of(new MappingScriptRepresentationCreator());

        default:
            return Optional.empty();
        }
    }

    /**
     * Saves the given consumer API definition provided that it is consistent.
     * 
     * @param referencedHistory        The history name of the referenced provider
     *                                 definition
     * @param referencedRevisionNumber The revision number of the referenced
     *                                 provider definition
     * @param consumerName             The name of the consumer
     * @param apiDefinition            The API definition to save
     * @return The persisted API definition
     */
    @Transactional
    public PersistentConsumerApiDefinition saveConsumerApi(String referencedHistory, int referencedRevisionNumber, String consumerName,
            String apiDefinition) {

        // Try to load the referenced provider API definition
        PersistentProviderApiDefinition referencedDefinition = this.providerApisService
                .readApiRevision(referencedHistory, referencedRevisionNumber)
                .orElseThrow(() -> new ApiProcessingException("The referenced revision does not exist."));

        ProviderApiDefinition referencedRevision = ProviderApiLoader.loadFromString(referencedDefinition.getRevisionNumber(),
                referencedDefinition.getDefinitionText(), true, Optional.empty());

        // Parse the given client API definition
        ConsumerApiDefinition consumerDefinition;
        try {
            consumerDefinition = ConsumerApiLoader.loadFromString(apiDefinition, referencedDefinition.getHistoryName(),
                    referencedRevisionNumber);
        } catch (APIParseException e) {
            throw new ApiProcessingException("Error processing API definition: " + e.getMessage(), e);
        }

        // Attempt to resolve the two revisions against each other
        try {
            RevisionHistory revisionHistory = new RevisionHistory(referencedRevision);

            // Check if the desired revision is currently supported
            LocalDateTime currentTime = LocalDateTime.now();
            Set<Integer> supportedRevisions;
            if ((currentTime.compareTo(referencedDefinition.getSupportedFrom()) >= 0) &&
                    currentTime.compareTo(referencedDefinition.getSupportedUntil()) < 0) {
                // The revision is supported, mark it as such
                supportedRevisions = Collections.singleton(referencedRevision.getRevision());
            } else {
                // The revision is not supported
                supportedRevisions = Collections.emptySet();
            }

            new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerDefinition);
        } catch (DefinitionResolutionException e) {
            throw new ApiProcessingException("The definition is incompatible to the referenced provider definition: " + e.getMessage(), e);
        }

        // If no errors occur, the consumer definition can be saved
        PersistentConsumerApiDefinition persistentConsumerDefinition = new PersistentConsumerApiDefinition();
        persistentConsumerDefinition.setConsumerName(consumerName);
        persistentConsumerDefinition.setReferencedRevision(referencedDefinition);
        persistentConsumerDefinition.setDefinitionText(apiDefinition);

        this.apisRepository.saveConsumerApiDefinition(persistentConsumerDefinition);

        return persistentConsumerDefinition;
    }

}
