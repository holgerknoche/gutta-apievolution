package gutta.apievolution.fixedformat.objectmapping;

class EnumMapper implements TypeMapper {

    private final Class<?> enumType;
    
    public EnumMapper(Class<?> enumType) {
        this.enumType = enumType;
    }

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
        int ordinal = ((Enum<?>) value).ordinal();
        data.writeInt32(ordinal);
    }

}
