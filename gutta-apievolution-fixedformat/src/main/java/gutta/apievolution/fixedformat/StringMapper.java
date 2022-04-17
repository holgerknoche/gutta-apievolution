package gutta.apievolution.fixedformat;

class StringMapper implements TypeMapper {

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
    public void writeValue(Object value, FixedFormatData data) {
        data.writeBoundedString((String) value, this.maxLength);
    }

}
