package gutta.apievolution.repository;

import org.hibernate.annotations.LazyToOne;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "ConsumerApiDefinitions")
public class PersistentConsumerApiDefinition {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "seq_consumer_apis")
    private Integer id;

    private LocalDateTime commitTime;

    @ManyToOne
    private PersistentProviderApiDefinition referencedRevision;

    @Lob
    private String definitionText;

    public Integer getId() {
        return this.id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public LocalDateTime getCommitTime() {
        return this.commitTime;
    }

    public void setCommitTime(LocalDateTime commitTime) {
        this.commitTime = commitTime;
    }

    public PersistentProviderApiDefinition getReferencedRevision() {
        return this.referencedRevision;
    }

    public void setReferencedRevision(PersistentProviderApiDefinition referencedRevision) {
        this.referencedRevision = referencedRevision;
    }

    public String getDefinitionText() {
        return this.definitionText;
    }

    public void setDefinitionText(String definitionText) {
        this.definitionText = definitionText;
    }

}
