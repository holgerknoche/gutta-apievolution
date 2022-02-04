package gutta.apievolution.repository;

import java.time.LocalDateTime;
import javax.persistence.*;

/**
 * JPA entity for a supported revision entry.
 */
@Entity
public class PersistentSupportedRevision {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "seq_supported_revisions")
    private Integer id;

    @ManyToOne
    private PersistentProviderApiDefinition referencedRevision;

    private LocalDateTime commitTime;

    private LocalDateTime supportedFrom;

    private LocalDateTime supportedUntil;

    /**
     * Returns this revision entry's ID.
     * @return see above
     */
    public Integer getId() {
        return this.id;
    }

    /**
     * Sets this revision entry's ID.
     * @param id The ID to set
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * Returns the provider revision referenced by this entry.
     * @return see above
     */
    public PersistentProviderApiDefinition getReferencedRevision() {
        return this.referencedRevision;
    }

    /**
     * Sets the provider revision referenced by this entry.
     * @param referencedRevision The revision to set
     */
    public void setReferencedRevision(PersistentProviderApiDefinition referencedRevision) {
        this.referencedRevision = referencedRevision;
    }

    /**
     * Returns the commit time of this entry.
     * @return see above
     */
    public LocalDateTime getCommitTime() {
        return this.commitTime;
    }

    /**
     * Sets the commit time of this entry.
     * @param commitTime The commit time to set
     */
    public void setCommitTime(LocalDateTime commitTime) {
        this.commitTime = commitTime;
    }

    /**
     * Returns the time from which the referenced revision is supported.
     * @return see above
     */
    public LocalDateTime getSupportedFrom() {
        return this.supportedFrom;
    }

    /**
     * Sets the time from which the referenced revision is supported.
     * @param supportedFrom The time to set
     */
    public void setSupportedFrom(LocalDateTime supportedFrom) {
        this.supportedFrom = supportedFrom;
    }

    /**
     * Returns the time to which the referenced revision is supported.
     * @return see above
     */
    public LocalDateTime getSupportedUntil() {
        return this.supportedUntil;
    }

    /**
     * Sets the time until the referenced revision is supported.
     * @param supportedUntil The time to set
     */
    public void setSupportedUntil(LocalDateTime supportedUntil) {
        this.supportedUntil = supportedUntil;
    }

}
