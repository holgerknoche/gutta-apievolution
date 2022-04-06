package gutta.apievolution.repository;

import org.jboss.logging.Logger;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.NonUniqueResultException;
import javax.persistence.TypedQuery;
import javax.transaction.Transactional;

/**
 * JPA Repository for managing persistent provider API definitions.
 */
@ApplicationScoped
public class ProviderApisRepository {

    @Inject
    Logger logger;

    @Inject
    EntityManager entityManager;

    /**
     * Finds a provider API revision by its history name and revision number.
     * 
     * @param historyName    The desired revision's history name
     * @param revisionNumber The desired revision's revision number
     * @return The definition, if it exists
     */
    public Optional<PersistentProviderApiDefinition> findByRevision(String historyName, int revisionNumber) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentProviderApiDefinition> query = em.createQuery(
                "select def from PersistentProviderApiDefinition def " +
                        "where def.historyName = :historyName and def.revisionNumber = :revisionNumber",
                PersistentProviderApiDefinition.class);
        query.setParameter("historyName", historyName);
        query.setParameter("revisionNumber", revisionNumber);

        try {
            PersistentProviderApiDefinition definition = query.getSingleResult();
            em.detach(definition);
            return Optional.of(definition);
        } catch (NoResultException | NonUniqueResultException e) {
            this.logger.warnf("Error retrieving revision %d in history '%s'.", revisionNumber, historyName, e);
            return Optional.empty();
        }
    }

    /**
     * Returns all revisions in the given history.
     * 
     * @param historyName The name of the history
     * @return The revisions in the history, ordered by revision number. If the
     *         history does not exist, an empty list is returned
     */
    public List<PersistentProviderApiDefinition> findApiDefinitionsInHistory(String historyName) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentProviderApiDefinition> query = em.createQuery(
                "select def from PersistentProviderApiDefinition def where def.historyName = :historyName " +
                        "order by def.revisionNumber",
                PersistentProviderApiDefinition.class);
        query.setParameter("historyName", historyName);

        List<PersistentProviderApiDefinition> definitions = query.getResultList();
        definitions.forEach(em::detach);

        return definitions;
    }

    /**
     * Finds the revision numbers that were supported in the given history at the
     * given time.
     * 
     * @param historyName The name of the history
     * @param atTime      The time at which the supported revisions should be
     *                    determined
     * @return A set of the supported revision numbers
     */
    public Set<Integer> findSupportedRevisions(String historyName, LocalDateTime atTime) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentProviderApiDefinition> query = em.createQuery(
                "select def from PersistentProviderApiDefinition def where def.historyName = :historyName " +
                        "and def.supportedFrom <= :atTime and def.supportedUntil > :atTime " +
                        "order by def.revisionNumber",
                PersistentProviderApiDefinition.class);
        query.setParameter("historyName", historyName);
        query.setParameter("atTime", atTime);

        List<PersistentProviderApiDefinition> definitions = query.getResultList();
        return definitions.stream().map(PersistentProviderApiDefinition::getRevisionNumber).collect(Collectors.toSet());
    }

    /**
     * Saves the given definition to the repository. As part of this process, the
     * commit timestamp is set.
     * 
     * @param definition The definition to save
     */
    @Transactional
    public void saveDefinition(PersistentProviderApiDefinition definition) {
        EntityManager em = this.entityManager;

        em.persist(definition);
    }

}
