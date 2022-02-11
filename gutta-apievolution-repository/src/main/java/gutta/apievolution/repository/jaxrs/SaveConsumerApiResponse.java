package gutta.apievolution.repository.jaxrs;

import java.time.LocalDateTime;

class SaveConsumerApiResponse {

    public final Integer id;

    public final LocalDateTime commitTime;

    SaveConsumerApiResponse(Integer id, LocalDateTime commitTime) {
        this.id = id;
        this.commitTime = commitTime;
    }

}
