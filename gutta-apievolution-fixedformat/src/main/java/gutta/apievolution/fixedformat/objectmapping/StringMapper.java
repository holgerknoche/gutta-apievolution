package gutta.apievolution.fixedformat.objectmapping;

class StringMapper implements TypeMapper<String> {

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
    public int getMaxLength() {
        return this.maxLength;
    }

    @Override
    public String readValue(FixedFormatData data) {
        return data.readBoundedString(this.maxLength);
    }
    
    @Override
    public void writeValue(Object value, FixedFormatData data) {
        data.writeBoundedString((String) value, this.maxLength);
    }

}
