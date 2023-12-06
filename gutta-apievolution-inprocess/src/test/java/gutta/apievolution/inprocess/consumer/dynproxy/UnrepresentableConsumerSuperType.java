package gutta.apievolution.inprocess.consumer.dynproxy;

public class UnrepresentableConsumerSuperType implements ConsumerSuperType {

    public static final UnrepresentableConsumerSuperType INSTANCE = new UnrepresentableConsumerSuperType();
    
    private UnrepresentableConsumerSuperType() {
        // Private constructor
    }
    
    @Override
    public Integer getInheritedField() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setInheritedField(Integer inheritedField) {
        throw new UnsupportedOperationException();        
    }

}
