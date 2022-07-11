package gutta.apievolution.fixedformat.objectmapping;

interface TypeMapper<T> {

    boolean isCacheable(); 
    
    int getMaxLength();
    
    T readValue(FixedFormatData data);
    
    void writeValue(Object value, FixedFormatData data);
    
}
