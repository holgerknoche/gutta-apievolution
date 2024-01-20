package gutta.apievolution.fixedformat.objectmapping;

interface TypeMapper<T> {

    boolean isCacheable(); 
    
    int getMaxLength();
    
    T readValue(FixedFormatData data);
    
    T handleUnrepresentableValue();
            
    default boolean isUnrepresentable(Object value) {
        return false;
    }
    
    void writeValue(Object value, FixedFormatData data);
        
}
