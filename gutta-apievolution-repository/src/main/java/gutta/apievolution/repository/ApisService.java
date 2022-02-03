package gutta.apievolution.repository;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.dsl.APIParseException;
import gutta.apievolution.dsl.ProviderApiLoader;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@ApplicationScoped
public class ApisService {

    @Inject
    ApisRepository apisRepository;

    public String readApiRevision(String historyName, int revisionNumber) {
        PersistentApiDefinition apiDefinition = this.apisRepository.findByRevision(historyName, revisionNumber);
        return apiDefinition.getDefinitionText();
    }

    @Transactional
    public void saveApiRevision(String historyName, String apiDefinition) {
        List<PersistentApiDefinition> existingDefinitions =
                this.apisRepository.findApiDefinitionsInHistory(historyName);

        if (existingDefinitions.isEmpty()) {
            this.createNewHistory(historyName, apiDefinition);
        } else {
            PersistentApiDefinition previousRevision = existingDefinitions.get(existingDefinitions.size() - 1);
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

        PersistentApiDefinition definition = new PersistentApiDefinition();
        definition.setHistoryName(historyName);
        definition.setRevisionNumber(revisionNumber);
        definition.setDefinitionText(apiDefinition);

        this.apisRepository.saveDefinition(definition);
    }

    private void appendRevisionToHistory(String historyName, PersistentApiDefinition previousRevision,
                                         String apiDefinition) {

    }

}
