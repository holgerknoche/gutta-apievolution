package gutta.apievolution.fixedformat.objectmapping;

interface TypeMapper {

    boolean isCacheable(); 
    
    int getMaxLength();
    
    void writeValue(Object value, FixedFormatData data);
    
}
