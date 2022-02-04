package gutta.apievolution.repository;

import java.time.LocalDateTime;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

/**
 * JPA repository for consumer API definitions.
 */
@ApplicationScoped
public class ConsumerApisRepository {

    @Inject
    EntityManager entityManager;

    /**
     * Saves the given definition to the repository. As part of this process, the commit timestamp is set.
     * @param definition The definition to save
     */
    @Transactional
    public void saveConsumerApiDefinition(PersistentConsumerApiDefinition definition) {
        definition.setCommitTime(LocalDateTime.now());
        this.entityManager.persist(definition);
    }

}
