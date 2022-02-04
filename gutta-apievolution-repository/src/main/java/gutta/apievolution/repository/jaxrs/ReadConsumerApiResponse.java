package gutta.apievolution.repository.jaxrs;

import java.time.LocalDateTime;

class ReadConsumerApiResponse {

    public final Integer id;

    public final LocalDateTime commitTime;

    public final String consumerName;

    public final String referencedHistoryName;

    public final int referencedRevisionNumber;

    public final String definition;

    public ReadConsumerApiResponse(Integer id, LocalDateTime commitTime, String consumerName,
                                   String referencedHistoryName, int referencedRevisionNumber, String definition) {
        this.id = id;
        this.commitTime = commitTime;
        this.consumerName = consumerName;
        this.referencedHistoryName = referencedHistoryName;
        this.referencedRevisionNumber = referencedRevisionNumber;
        this.definition = definition;
    }

}
