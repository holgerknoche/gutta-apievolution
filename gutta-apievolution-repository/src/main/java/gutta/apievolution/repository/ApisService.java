package gutta.apievolution.repository;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.List;

@ApplicationScoped
public class ApisService {

    @Inject
    ApisRepository apisRepository;

    @Transactional
    public void saveApiRevision(String historyName, String apiDefinition) {
        List<PersistentApiDefinition> existingDefinitions =
                this.apisRepository.findApiDefinitionsInHistory(historyName);

        // TODO Determine the next revision from the revision historyq

        System.out.println(existingDefinitions);
    }

}
