package gutta.apievolution.fixedformat;

interface TypeMapper {

    boolean isCacheable(); 
    
    int getMaxLength();
    
    void writeValue(Object value, FixedFormatData data);
    
}
