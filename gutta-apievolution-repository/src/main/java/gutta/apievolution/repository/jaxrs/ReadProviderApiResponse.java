package gutta.apievolution.repository.jaxrs;

import java.time.LocalDateTime;

class ReadProviderApiResponse {

    public final String historyName;

    public final int revisionNumber;

    public final LocalDateTime commitTime;

    public final String definition;

    public ReadProviderApiResponse(String historyName, int revisionNumber, LocalDateTime commitTime,
                                   String definition) {
        this.historyName = historyName;
        this.revisionNumber = revisionNumber;
        this.commitTime = commitTime;
        this.definition = definition;
    }

}
