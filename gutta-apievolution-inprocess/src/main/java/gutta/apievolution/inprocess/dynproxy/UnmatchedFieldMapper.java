package gutta.apievolution.inprocess.dynproxy;

import java.lang.reflect.Method;

class UnmatchedFieldMapper extends ReflectiveFieldMapper {

    public UnmatchedFieldMapper(Method fieldAccessor) {
        super(fieldAccessor);
    }
    
    @Override
    protected Object mapValue(Object value) {
        // Always return null, as the field cannot be present
        return null;
    }

}
