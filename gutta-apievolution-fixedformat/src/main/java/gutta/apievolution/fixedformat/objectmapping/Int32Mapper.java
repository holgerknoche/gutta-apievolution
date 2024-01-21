package gutta.apievolution.fixedformat.objectmapping;

class Int32Mapper extends TypeMapper<Integer> {

    @Override
    public boolean isCacheable() {
        return true;
    }
    
    @Override
    public int getDataLength() {
        return 4;
    }
    
    @Override
    public Integer readRegularValue(FixedFormatData data) {
        return data.readInt32();
    }
    
    @Override
    public Integer handleUnrepresentableValue() {
        throw new IllegalStateException("The basic type int32 does not support unrepresentable values.");
    }
    
    @Override
    public void writeRegularValue(Object value, FixedFormatData data) {
        int intValue = (Integer) value;
        data.writeInt32(intValue);
    }

}
