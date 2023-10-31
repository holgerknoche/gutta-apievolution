package gutta.apievolution.inprocess.dynproxy;

class BasicTypeValueMapper implements ValueMapper {
    
    @Override
    public Object mapValue(Object value) {
        // True basic types need no mapping
        return value;
    }
    
}
