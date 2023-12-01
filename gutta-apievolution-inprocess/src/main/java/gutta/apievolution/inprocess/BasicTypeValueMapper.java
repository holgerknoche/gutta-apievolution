package gutta.apievolution.inprocess;

class BasicTypeValueMapper implements ValueMapper {

    @Override
    public Object mapValue(Object value) {
        // True basic types need no mapping
        return value;
    }

}
