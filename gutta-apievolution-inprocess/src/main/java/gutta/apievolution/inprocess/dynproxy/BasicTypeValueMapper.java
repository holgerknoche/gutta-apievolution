package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.ValueMapper;

class BasicTypeValueMapper implements ValueMapper {
    
    @Override
    public Object mapValue(Object value) {
        // True basic types need no mapping
        return value;
    }
    
}
