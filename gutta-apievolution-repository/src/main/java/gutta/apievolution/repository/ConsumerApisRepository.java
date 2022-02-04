package gutta.apievolution.repository;

import java.time.LocalDateTime;
import java.util.Optional;
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
     * Finds a consumer API definition by its ID.
     * @param id The ID to look for
     * @return The consumer API definition, if it exists
     */
    public Optional<PersistentConsumerApiDefinition> findById(Integer id) {
        EntityManager em = this.entityManager;

        PersistentConsumerApiDefinition definition = em.find(PersistentConsumerApiDefinition.class, id);
        return Optional.ofNullable(definition);
    }

    /**
     * Saves the given definition to the repository. As part of this process, the commit timestamp is set.
     * @param definition The definition to save
     */
    @Transactional
    public void saveConsumerApiDefinition(PersistentConsumerApiDefinition definition) {
        EntityManager em = this.entityManager;

        definition.setCommitTime(LocalDateTime.now());
        em.persist(definition);
        em.detach(definition);
    }

}
