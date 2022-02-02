package gutta.apievolution.repository;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.Entity;
import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import java.util.List;

@ApplicationScoped
public class ApisRepository {

    @Inject
    EntityManager entityManager;

    public List<PersistentApiDefinition> findApiDefinitionsInHistory(String historyName) {
        EntityManager em = this.entityManager;

        TypedQuery<PersistentApiDefinition> query = em.createQuery(
                "select def from PersistentApiDefinition def where def.historyName = :historyName " +
                        "order by def.revisionNumber",
                PersistentApiDefinition.class);
        query.setParameter("historyName", historyName);

        List<PersistentApiDefinition> definitions = query.getResultList();
        definitions.forEach(definition -> em.detach(definition));

        return definitions;
    }

}
