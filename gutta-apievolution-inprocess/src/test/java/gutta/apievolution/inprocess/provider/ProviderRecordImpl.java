package gutta.apievolution.inprocess.provider;

public class ProviderRecordImpl implements ProviderRecord {

    private Integer field;

    public ProviderRecordImpl() {
        // Parameterless constructor
    }

    ProviderRecordImpl(Integer field) {
        this.field = field;
    }

    @Override
    public Integer getField() {
        return this.field;
    }

    public void setField(Integer field) {
        this.field = field;
    }

}
