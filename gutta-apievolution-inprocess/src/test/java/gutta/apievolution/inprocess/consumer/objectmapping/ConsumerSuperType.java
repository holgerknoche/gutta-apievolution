package gutta.apievolution.inprocess.consumer.objectmapping;

import gutta.apievolution.inprocess.UnrepresentableValue;

public abstract class ConsumerSuperType {

    private Integer inheritedField;
    
    public Integer getInheritedField() {
        return this.inheritedField;
    }
    
    public void setInheritedField(Integer inheritedField) {
        this.inheritedField = inheritedField;
    }
    
    public boolean isRepresentable() {
        return true;
    }
    
    @UnrepresentableValue
    public static ConsumerSuperType unrepresentableValue() {
        return Unrepresentable.INSTANCE;
    }
    
    private static class Unrepresentable extends ConsumerSuperType {
        
        private static final Unrepresentable INSTANCE = new Unrepresentable();
        
        @Override
        public boolean isRepresentable() {
            return false;
        }
        
    }
    
}
