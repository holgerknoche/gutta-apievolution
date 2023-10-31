package gutta.apievolution.inprocess.dynproxy;

import java.lang.reflect.Method;

class BasicTypeFieldMapper extends ReflectiveFieldMapper {

    public BasicTypeFieldMapper(Method accessorMethod) {
        super(accessorMethod);
    }
    
    @Override
    protected Object mapValue(Object value) {
        // Basic types do not need to be mapped
        return value;
    }
    
}
