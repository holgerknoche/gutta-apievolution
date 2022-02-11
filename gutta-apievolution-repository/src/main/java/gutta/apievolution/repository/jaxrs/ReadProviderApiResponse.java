package gutta.apievolution.repository.jaxrs;

import java.time.LocalDateTime;

class ReadProviderApiResponse {

    public final String historyName;

    public final int revisionNumber;

    public final LocalDateTime commitTime;

    public final LocalDateTime supportedFrom;

    public final LocalDateTime supportedUntil;

    public final String definition;

    public ReadProviderApiResponse(String historyName, int revisionNumber, LocalDateTime commitTime,
                                   LocalDateTime supportedFrom, LocalDateTime supportedUntil, String definition) {
        this.historyName = historyName;
        this.revisionNumber = revisionNumber;
        this.commitTime = commitTime;
        this.supportedFrom = supportedFrom;
        this.supportedUntil = supportedUntil;
        this.definition = definition;
    }

}
