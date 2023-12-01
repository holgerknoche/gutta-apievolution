package gutta.apievolution.inprocess.consumer.dynproxy;

class ConsumerRecordImpl implements ConsumerRecord {

    private Integer field;

    @Override
    public Integer getField() {
        return this.field;
    }

    public void setField(Integer field) {
        this.field = field;
    }

}
