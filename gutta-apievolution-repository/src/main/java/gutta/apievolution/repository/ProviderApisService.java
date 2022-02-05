package gutta.apievolution.repository;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.dsl.APIParseException;
import gutta.apievolution.dsl.APIResolutionException;
import gutta.apievolution.dsl.ProviderApiLoader;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;

/**
 * Service for accessing and managing provider API definitions.
 */
@ApplicationScoped
public class ProviderApisService {

    @Inject
    ProviderApisRepository apisRepository;

    /**
     * Reads an API definition given its history name and revision number.
     * @param historyName The history name of the desired definition
     * @param revisionNumber The revision number of the desired definition
     * @return The definition, if it exists
     */
    public Optional<PersistentProviderApiDefinition> readApiRevision(String historyName, int revisionNumber) {
        return this.apisRepository.findByRevision(historyName, revisionNumber);
    }

    /**
     * Saves an API definition to the given history, appending it if the history already exists.
     * @param historyName The name of the history to add this definition to
     * @param supportedFrom The timestamp from which the revision is supported. If {@code  null}, it will
     *                      be replaced by the current timestamp
     * @param supportedUntil The timestamp until which the revision is supported. If {@code null}, it will
     *                       be replaced by the max timestamp
     * @param apiDefinition The definition to save
     */
    @Transactional
    public void saveApiRevision(String historyName, LocalDateTime supportedFrom, LocalDateTime supportedUntil,
                                String apiDefinition) {
        List<PersistentProviderApiDefinition> existingDefinitions =
                this.apisRepository.findApiDefinitionsInHistory(historyName);

        if (existingDefinitions.isEmpty()) {
            this.createNewHistory(historyName, supportedFrom, supportedUntil, apiDefinition);
        } else {
            PersistentProviderApiDefinition previousRevision = existingDefinitions.get(existingDefinitions.size() - 1);
            this.appendRevisionToHistory(historyName, previousRevision, supportedFrom, supportedUntil, apiDefinition);
        }
    }

    private void createNewHistory(String historyName, LocalDateTime supportedFrom, LocalDateTime supportedUntil,
                                  String apiDefinition) {
        int revisionNumber = 0;

        // Try to load the definition to check for syntax errors
        try {
            ProviderApiLoader.loadFromString(revisionNumber, apiDefinition,
                    true, Optional.empty());
        } catch (APIParseException e) {
            throw new ApiProcessingException("Error processing API definition: " + e.getMessage(), e);
        }

        this.saveRevision(historyName, revisionNumber, supportedFrom, supportedUntil, apiDefinition);
    }

    private void appendRevisionToHistory(String historyName, PersistentProviderApiDefinition previousRevision,
                                         LocalDateTime supportedFrom, LocalDateTime supportedUntil,
                                         String apiDefinition) {
        int revisionNumber = previousRevision.getRevisionNumber() + 1;

        // Parse the previous revision and the new one to check replacements
        try {
            // Load the previous revision ignoring replacements (if any), so we do not have to load
            // the entire history
            ProviderApiDefinition predecessor = ProviderApiLoader.loadFromString(
                    previousRevision.getRevisionNumber(),
                    previousRevision.getDefinitionText(),
                    true,
                    Optional.empty()
            );

            ProviderApiLoader.loadFromString(
                    revisionNumber,
                    apiDefinition,
                    false,
                    Optional.of(predecessor)
            );
        } catch (APIParseException | APIResolutionException e) {
            throw new ApiProcessingException("Error processing API definition: " + e.getMessage(), e);
        }

        this.saveRevision(historyName, revisionNumber, supportedFrom, supportedUntil, apiDefinition);
    }

    private void saveRevision(String historyName, int revisionNumber, LocalDateTime supportedFrom,
                              LocalDateTime supportedUntil, String apiDefinition) {
        LocalDateTime actualSupportedFrom = (supportedFrom == null) ? LocalDateTime.now() : supportedFrom;
        LocalDateTime actualSupportedUntil = (supportedUntil == null) ? LocalDateTime.MAX : supportedUntil;

        PersistentProviderApiDefinition definition = new PersistentProviderApiDefinition();
        definition.setHistoryName(historyName);
        definition.setRevisionNumber(revisionNumber);
        definition.setSupportedFrom(actualSupportedFrom);
        definition.setSupportedUntil(actualSupportedUntil);
        definition.setDefinitionText(apiDefinition);

        this.apisRepository.saveDefinition(definition);
    }

}
