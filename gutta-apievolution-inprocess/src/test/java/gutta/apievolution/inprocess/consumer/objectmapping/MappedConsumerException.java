package gutta.apievolution.inprocess.consumer.objectmapping;

public class MappedConsumerException extends Exception {

    private static final long serialVersionUID = -6265239311354919342L;

    private String exceptionField;

    public String getExceptionField() {
        return exceptionField;
    }

    public void setExceptionField(String exceptionField) {
        this.exceptionField = exceptionField;
    }

}
