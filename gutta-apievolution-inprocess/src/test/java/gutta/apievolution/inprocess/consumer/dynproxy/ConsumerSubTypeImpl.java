package gutta.apievolution.inprocess.consumer.dynproxy;

public class ConsumerSubTypeImpl implements ConsumerSubType {

    private Integer inheritedField;
    
    private Integer subField;
    
    @Override
    public Integer getInheritedField() {
        return this.inheritedField;
    }

    @Override
    public void setInheritedField(Integer inheritedField) {
        this.inheritedField = inheritedField;
    }

    @Override
    public Integer getSubField() {
        return this.subField;
    }

    @Override
    public void setSubField(Integer subField) {
        this.subField = subField;
    }

}
