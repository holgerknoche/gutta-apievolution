package gutta.apievolution.fixedformat.objectmapping;

class Int32Mapper implements TypeMapper<Integer> {

    @Override
    public boolean isCacheable() {
        return true;
    }
    
    @Override
    public int getMaxLength() {
        return 4;
    }
    
    @Override
    public Integer readValue(FixedFormatData data) {
        return data.readInt32();
    }
    
    @Override
    public Integer handleUnrepresentableValue() {
        throw new IllegalStateException("The basic type int32 does not support unrepresentable values.");
    }
    
    @Override
    public void writeValue(Object value, FixedFormatData data) {
        int intValue = (value == null) ? 0 : (Integer) value;
        data.writeInt32(intValue);
    }

}
