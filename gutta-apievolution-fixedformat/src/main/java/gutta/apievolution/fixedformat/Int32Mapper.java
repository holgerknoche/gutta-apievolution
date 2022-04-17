package gutta.apievolution.fixedformat;

class Int32Mapper implements TypeMapper {

    @Override
    public boolean isCacheable() {
        return true;
    }
    
    @Override
    public int getMaxLength() {
        return 4;
    }

    @Override
    public void writeValue(Object value, FixedFormatData data) {
        int intValue = (value == null) ? 0 : (Integer) value;
        data.writeInt32(intValue);
    }

}
