package gutta.apievolution.fixedformat.objectmapping;

abstract class UserDefinedTypeMapper extends TypeMapper<Object> {
    
    @Override
    protected boolean isCacheable() {
        return true;
    }

}
