package gutta.apievolution.fixedformat.objectmapping;

class StringMapper extends TypeMapper<String> {

    private final int maxLength;
    
    public StringMapper(int maxLength) {
        this.maxLength = maxLength;
    }
    
    @Override
    public boolean isCacheable() {
        // Mappers for parameterized types are not cacheable
        return false;
    }
    
    @Override
    protected int getDataLength() {
        return this.maxLength;
    }

    @Override
    protected String readRegularValue(FixedFormatData data) {
        return data.readBoundedString(this.maxLength);
    }
    
    @Override
    public String handleUnrepresentableValue() {
        throw new IllegalStateException("String types cannot have unrepresentable values.");  
    }
    
    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {
        data.writeBoundedString((String) value, this.maxLength);
    }

}
