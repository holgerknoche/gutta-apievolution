package gutta.apievolution.repository;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "ApiDefinitions")
public class PersistentProviderApiDefinition {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "seq_apis")
    private Integer id;

    private LocalDateTime commitTime;

    private String historyName;

    private Integer revisionNumber;

    @Lob
    private String definitionText;

    public Integer getId() {
        return this.id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getHistoryName() {
        return this.historyName;
    }

    public void setHistoryName(String historyName) {
        this.historyName = historyName;
    }

    public Integer getRevisionNumber() {
        return this.revisionNumber;
    }

    public void setRevisionNumber(Integer revisionNumber) {
        this.revisionNumber = revisionNumber;
    }

    public LocalDateTime getCommitTime() {
        return this.commitTime;
    }

    public void setCommitTime(LocalDateTime commitTime) {
        this.commitTime = commitTime;
    }

    public String getDefinitionText() {
        return this.definitionText;
    }

    public void setDefinitionText(String definitionText) {
        this.definitionText = definitionText;
    }

}
