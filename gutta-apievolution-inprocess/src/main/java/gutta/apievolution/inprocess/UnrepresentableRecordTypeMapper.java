package gutta.apievolution.inprocess;

class UnrepresentableRecordTypeMapper extends AbstractRecordTypeValueMapper {

    public UnrepresentableRecordTypeMapper(Class<?> targetType) {
        super(targetType);
    }

    @Override
    public boolean isRepresentable(Object value) {
        return false;
    }
    
    @Override
    public Object mapRepresentableValue(Object value) {
        throw new UnsupportedOperationException();
    }

}
