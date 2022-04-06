package gutta.apievolution.repository;

import java.time.LocalDateTime;
import javax.persistence.*;

/**
 * JPA entity for a provider API definition.
 */
@Entity
@Table(name = "ProviderApiDefinitions")
public class PersistentProviderApiDefinition {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "seq_provider_apis")
    private Integer id;

    private LocalDateTime commitTime;

    private String historyName;

    private Integer revisionNumber;

    private LocalDateTime supportedFrom;

    private LocalDateTime supportedUntil;

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
     * Returns the name of the revision history to which this definition belongs.
     * 
     * @return see above
     */
    public String getHistoryName() {
        return this.historyName;
    }

    /**
     * Sets the name of the revision history to which this definition belongs.
     * 
     * @param historyName The history name to set
     */
    public void setHistoryName(String historyName) {
        this.historyName = historyName;
    }

    /**
     * Returns the revision number of this definition within the history.
     * 
     * @return see above
     */
    public Integer getRevisionNumber() {
        return this.revisionNumber;
    }

    /**
     * Sets the revision number of this definition within the history.
     * 
     * @param revisionNumber The revision number to set
     */
    public void setRevisionNumber(Integer revisionNumber) {
        this.revisionNumber = revisionNumber;
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
     * Sets the timestamp at which this definition was committed.
     * 
     * @param commitTime The commit timestamp to set
     */
    public void setCommitTime(LocalDateTime commitTime) {
        this.commitTime = commitTime;
    }

    /**
     * Returns the timestamp from which this revision is supported.
     * 
     * @return see above
     */
    public LocalDateTime getSupportedFrom() {
        return this.supportedFrom;
    }

    /**
     * Sets the timestamp from which this revision is supported.
     * 
     * @param supportedFrom The timestamp to set
     */
    public void setSupportedFrom(LocalDateTime supportedFrom) {
        this.supportedFrom = supportedFrom;
    }

    /**
     * Returns the timestamp until which this revision is supported.
     * 
     * @return see above
     */
    public LocalDateTime getSupportedUntil() {
        return this.supportedUntil;
    }

    /**
     * Sets the timestamp until which this revision is supported.
     * 
     * @param supportedUntil The timestamp to set
     */
    public void setSupportedUntil(LocalDateTime supportedUntil) {
        this.supportedUntil = supportedUntil;
    }

    /**
     * Returns the definition text of this provider API definition.
     * 
     * @return see above
     */
    public String getDefinitionText() {
        return this.definitionText;
    }

    /**
     * Sets the definition text of this provider API definition.
     * 
     * @param definitionText The definition text to set
     */
    public void setDefinitionText(String definitionText) {
        this.definitionText = definitionText;
    }

}
