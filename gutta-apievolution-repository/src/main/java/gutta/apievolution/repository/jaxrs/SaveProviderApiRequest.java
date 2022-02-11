package gutta.apievolution.repository.jaxrs;

import java.time.LocalDateTime;

class SaveProviderApiRequest {

    public final LocalDateTime supportedFrom;

    public final LocalDateTime supportedUntil;

    public final String definition;

    public SaveProviderApiRequest(LocalDateTime supportedFrom, LocalDateTime supportedUntil, String definition) {
        this.supportedFrom = supportedFrom;
        this.supportedUntil = supportedUntil;
        this.definition = definition;
    }

}
