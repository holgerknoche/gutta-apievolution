package gutta.apievolution.repository;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.NonUniqueResultException;
import javax.persistence.TypedQuery;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.List;

@ApplicationScoped
public class ApisRepository {

    @Inject
    EntityManager entityManager;

    public PersistentApiDefinition findByRevision(String historyName, int revisionNumber) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentApiDefinition> query = em.createQuery("select def from PersistentApiDefinition def " +
                "where def.historyName = :historyName and def.revisionNumber = :revisionNumber",
                PersistentApiDefinition.class);
        query.setParameter("historyName", historyName);
        query.setParameter("revisionNumber", revisionNumber);

        try {
            return query.getSingleResult();
        } catch (NoResultException | NonUniqueResultException e) {
            throw new ApiProcessingException("No matching result was found.", e);
        }
    }

    public List<PersistentApiDefinition> findApiDefinitionsInHistory(String historyName) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentApiDefinition> query = em.createQuery(
                "select def from PersistentApiDefinition def where def.historyName = :historyName " +
                        "order by def.revisionNumber",
                PersistentApiDefinition.class);
        query.setParameter("historyName", historyName);

        List<PersistentApiDefinition> definitions = query.getResultList();
        definitions.forEach(em::detach);

        return definitions;
    }

    @Transactional
    public void saveDefinition(PersistentApiDefinition definition) {
        definition.setCommitTime(LocalDateTime.now());
        this.entityManager.persist(definition);
    }

}
