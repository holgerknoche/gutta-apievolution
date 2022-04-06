package gutta.apievolution.repository.jaxrs;

class SaveConsumerApiRequest {

    public final String referencedHistoryName;

    public final int referencedRevisionNumber;

    public final String consumerName;

    public final String definition;

    public SaveConsumerApiRequest(String referencedHistoryName, int referencedRevisionNumber, String consumerName,
            String definition) {
        this.referencedHistoryName = referencedHistoryName;
        this.referencedRevisionNumber = referencedRevisionNumber;
        this.consumerName = consumerName;
        this.definition = definition;
    }

}
