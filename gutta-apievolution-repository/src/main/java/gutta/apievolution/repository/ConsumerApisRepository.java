package gutta.apievolution.repository;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.persistence.EntityManager;
import jakarta.transaction.Transactional;

import java.time.LocalDateTime;
import java.util.Optional;

/**
 * JPA repository for consumer API definitions.
 */
@ApplicationScoped
public class ConsumerApisRepository {

    @Inject
    EntityManager entityManager;

    /**
     * Finds a consumer API definition by its ID.
     * 
     * @param id The ID to look for
     * @return The consumer API definition, if it exists
     */
    public Optional<PersistentConsumerApiDefinition> findById(Integer id) {
        EntityManager em = this.entityManager;

        PersistentConsumerApiDefinition definition = em.find(PersistentConsumerApiDefinition.class, id);
        em.detach(definition);

        return Optional.ofNullable(definition);
    }

    /**
     * Saves the given definition to the repository. As part of this process, the
     * commit timestamp is set.
     * 
     * @param definition The definition to save
     */
    @Transactional
    public void saveConsumerApiDefinition(PersistentConsumerApiDefinition definition) {
        EntityManager em = this.entityManager;

        definition.setCommitTime(LocalDateTime.now());
        em.persist(definition);
    }

}
