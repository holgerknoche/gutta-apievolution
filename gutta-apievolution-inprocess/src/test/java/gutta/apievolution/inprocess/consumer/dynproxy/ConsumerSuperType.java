package gutta.apievolution.inprocess.consumer.dynproxy;

import gutta.apievolution.inprocess.UnrepresentableValue;

public interface ConsumerSuperType {
    
    @UnrepresentableValue
    static ConsumerSuperType unrepresentableValue() {
        return UnrepresentableConsumerSuperType.INSTANCE;
    }
    
    Integer getInheritedField();
    
    void setInheritedField(Integer inheritedField);
    
}
