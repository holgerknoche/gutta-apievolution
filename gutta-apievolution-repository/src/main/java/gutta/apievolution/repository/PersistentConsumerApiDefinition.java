package gutta.apievolution.repository;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Lob;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import java.time.LocalDateTime;

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

    private String consumerName;

    @ManyToOne
    private PersistentProviderApiDefinition referencedRevision;

    @Lob
    private String definitionText;

    /**
     * Returns this definition's ID.
     * 
     * @return see above
     */
    public Integer getId() {
        return this.id;
    }

    /**
     * Sets this definition's ID.
     * 
     * @param id The ID to set
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * Returns the timestamp at which this definition was committed.
     * 
     * @return see above
     */
    public LocalDateTime getCommitTime() {
        return this.commitTime;
    }

    /**
     * Returns the consumer name of this definition.
     * 
     * @return see above
     */
    public String getConsumerName() {
        return this.consumerName;
    }

    /**
     * Sets the consumer name of this definition.
     * 
     * @param consumerName The consumer name to set
     */
    public void setConsumerName(String consumerName) {
        this.consumerName = consumerName;
    }

    /**
     * Sets the timestamp at which this definition was committed.
     * 
     * @param commitTime The timestamp to set
     */
    public void setCommitTime(LocalDateTime commitTime) {
        this.commitTime = commitTime;
    }

    /**
     * Returns the provider API revision referenced by this consumer definition.
     * 
     * @return see above
     */
    public PersistentProviderApiDefinition getReferencedRevision() {
        return this.referencedRevision;
    }

    /**
     * Sets the provider API revision referenced by this consumer definition.
     * 
     * @param referencedRevision see above
     */
    public void setReferencedRevision(PersistentProviderApiDefinition referencedRevision) {
        this.referencedRevision = referencedRevision;
    }

    /**
     * Returns the definition text of this consumer API definition.
     * 
     * @return see above
     */
    public String getDefinitionText() {
        return this.definitionText;
    }

    /**
     * Sets the definition text of this consumer API definition.
     * 
     * @param definitionText The definition text to set
     */
    public void setDefinitionText(String definitionText) {
        this.definitionText = definitionText;
    }

}
