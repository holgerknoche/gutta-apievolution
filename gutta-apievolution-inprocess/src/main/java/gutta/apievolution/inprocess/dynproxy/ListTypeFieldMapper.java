package gutta.apievolution.inprocess.dynproxy;

import java.lang.reflect.Method;

class ListTypeFieldMapper extends ReflectiveFieldMapper {
    
    public ListTypeFieldMapper(Method accessorMethod) {
        super(accessorMethod);
    }
    
    @Override
    protected Object mapValue(Object value) {
        // TODO Create a list proxy
        return null;
    }

}
