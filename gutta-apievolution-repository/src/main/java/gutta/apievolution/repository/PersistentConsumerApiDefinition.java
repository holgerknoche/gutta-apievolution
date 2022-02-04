package gutta.apievolution.repository;

import java.time.LocalDateTime;
import javax.persistence.*;

/**
 * JPA entity for a consumer API definition.
 */
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

    /**
     * Returns this definition's ID.
     * @return see above
     */
    public Integer getId() {
        return this.id;
    }

    /**
     * Sets this definition's ID.
     * @param id The ID to set
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * Returns the timestamp at which this definition was committed.
     * @return see above
     */
    public LocalDateTime getCommitTime() {
        return this.commitTime;
    }

    /**
     * Sets the timestamp at which this definition was committed.
     * @param commitTime The timestamp to set
     */
    public void setCommitTime(LocalDateTime commitTime) {
        this.commitTime = commitTime;
    }

    /**
     * Returns the provider API revision referenced by this consumer definition.
     * @return see above
     */
    public PersistentProviderApiDefinition getReferencedRevision() {
        return this.referencedRevision;
    }

    /**
     * Sets the provider API revision referenced by this consumer definition.
     * @param referencedRevision see above
     */
    public void setReferencedRevision(PersistentProviderApiDefinition referencedRevision) {
        this.referencedRevision = referencedRevision;
    }

    /**
     * Returns the definition text of this consumer API definition.
     * @return see above
     */
    public String getDefinitionText() {
        return this.definitionText;
    }

    /**
     * Sets the definition text of this consumer API definition.
     * @param definitionText The definition text to set
     */
    public void setDefinitionText(String definitionText) {
        this.definitionText = definitionText;
    }

}
