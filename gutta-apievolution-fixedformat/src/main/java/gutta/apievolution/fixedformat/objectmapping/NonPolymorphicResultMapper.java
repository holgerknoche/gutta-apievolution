package gutta.apievolution.fixedformat.objectmapping;

class NonPolymorphicResultMapper extends AbstractRecordTypeMapper {
    
    private final TypeMapper<?> resultTypeMapper;
        
    public NonPolymorphicResultMapper(Class<?> formalResultType, TypeMapper<?> resultTypeMapper) {
        super(formalResultType);
        
        this.resultTypeMapper = resultTypeMapper;
    }

    @Override
    protected int getDataLength() {
        return this.resultTypeMapper.getDataLength();
    }

    @Override
    protected Object handleAbsentValue(FixedFormatData data) {
        data.skipBytes(this.getDataLength());
        return ValueOrException.forNull();
    }
        
    @Override
    protected Object readRegularValue(FixedFormatData data) {
        Object value = this.resultTypeMapper.readRegularValue(data);
        return ValueOrException.forValue(value);
    }

    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {        
        this.resultTypeMapper.writeRegularValue(value, data);
    }

}
