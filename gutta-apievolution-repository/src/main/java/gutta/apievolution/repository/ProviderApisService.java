package gutta.apievolution.repository;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.dsl.APIParseException;
import gutta.apievolution.dsl.APIResolutionException;
import gutta.apievolution.dsl.ProviderApiLoader;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;

@ApplicationScoped
public class ProviderApisService {

    @Inject
    ProviderApisRepository apisRepository;

    public Optional<String> readApiRevision(String historyName, int revisionNumber) {
        Optional<PersistentProviderApiDefinition> apiDefinition = this.apisRepository.findByRevision(historyName,
                revisionNumber);

        if (apiDefinition.isPresent()) {
            return Optional.of(apiDefinition.get().getDefinitionText());
        } else {
            return Optional.empty();
        }
    }

    @Transactional
    public void saveApiRevision(String historyName, String apiDefinition) {
        List<PersistentProviderApiDefinition> existingDefinitions =
                this.apisRepository.findApiDefinitionsInHistory(historyName);

        if (existingDefinitions.isEmpty()) {
            this.createNewHistory(historyName, apiDefinition);
        } else {
            PersistentProviderApiDefinition previousRevision = existingDefinitions.get(existingDefinitions.size() - 1);
            this.appendRevisionToHistory(historyName, previousRevision, apiDefinition);
        }
    }

    private void createNewHistory(String historyName, String apiDefinition) {
        int revisionNumber = 0;

        // Try to load the definition to check for syntax errors
        try {
            ProviderApiLoader.loadFromString(revisionNumber, apiDefinition,
                    true, Optional.empty());
        } catch (APIParseException e) {
            throw new ApiProcessingException("Error processing API definition: " + e.getMessage(), e);
        }

        this.saveRevision(historyName, revisionNumber, apiDefinition);
    }

    private void appendRevisionToHistory(String historyName, PersistentProviderApiDefinition previousRevision,
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

        this.saveRevision(historyName, revisionNumber, apiDefinition);
    }

    private void saveRevision(String historyName, int revisionNumber, String apiDefinition) {
        PersistentProviderApiDefinition definition = new PersistentProviderApiDefinition();
        definition.setHistoryName(historyName);
        definition.setRevisionNumber(revisionNumber);
        definition.setDefinitionText(apiDefinition);

        this.apisRepository.saveDefinition(definition);
    }

}
