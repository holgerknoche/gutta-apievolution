package gutta.apievolution.inprocess.consumer.objectmapping;

public abstract class ConsumerSuperType {

    private Integer inheritedField;
    
    public Integer getInheritedField() {
        return this.inheritedField;
    }
    
    public void setInheritedField(Integer inheritedField) {
        this.inheritedField = inheritedField;
    }
    
}
