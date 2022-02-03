package gutta.apievolution.repository;

import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.NonUniqueResultException;
import javax.persistence.TypedQuery;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@ApplicationScoped
public class ProviderApisRepository {

    @Inject
    Logger logger;

    @Inject
    EntityManager entityManager;

    public Optional<PersistentProviderApiDefinition> findByRevision(String historyName, int revisionNumber) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentProviderApiDefinition> query = em.createQuery(
                "select def from PersistentProviderApiDefinition def " +
                "where def.historyName = :historyName and def.revisionNumber = :revisionNumber",
                PersistentProviderApiDefinition.class);
        query.setParameter("historyName", historyName);
        query.setParameter("revisionNumber", revisionNumber);

        try {
            return Optional.of(query.getSingleResult());
        } catch (NoResultException | NonUniqueResultException e) {
            this.logger.warnf("Error retrieving revision %d in history '%s'.", revisionNumber, historyName, e);
            return Optional.empty();
        }
    }

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

    @Transactional
    public void saveDefinition(PersistentProviderApiDefinition definition) {
        definition.setCommitTime(LocalDateTime.now());
        this.entityManager.persist(definition);
    }

}
