package gutta.apievolution.inprocess.provider;

public class ProviderException extends Exception {

    private static final long serialVersionUID = -8361810480377200811L;

    private String exceptionField;

    public String getExceptionField() {
        return exceptionField;
    }

    public void setExceptionField(String exceptionField) {
        this.exceptionField = exceptionField;
    }

}
