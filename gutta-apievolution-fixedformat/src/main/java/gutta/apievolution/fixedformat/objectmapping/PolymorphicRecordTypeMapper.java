package gutta.apievolution.fixedformat.objectmapping;

import java.util.Collection;
import java.util.Map;

class PolymorphicRecordTypeMapper extends TypeMapper<Object> {
    
    private static final int DISCRIMINATOR_SIZE = 4;

    private final int dataLength;
    
    private final RecordTypeMapper ownTypeMapper;
    
    private final Map<Integer, RecordTypeMapper> subTypeMappers;
    
    public PolymorphicRecordTypeMapper(RecordTypeMapper ownTypeMapper, Map<Integer, RecordTypeMapper> subTypeMappers) {
        this.ownTypeMapper = ownTypeMapper;
        this.subTypeMappers = subTypeMappers;
        this.dataLength = determineMaxDataLength(subTypeMappers.values());
    }
    
    private static int determineMaxDataLength(Collection<RecordTypeMapper> mappers) {
        int maxLength = 0;
        
        for (RecordTypeMapper mapper : mappers) {
            if (mapper.getMaxLength() > maxLength) {
                maxLength = mapper.getDataLength();
            }
        }
        
        return (maxLength + DISCRIMINATOR_SIZE);
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    protected int getDataLength() {
        return this.dataLength;
    }

    @Override()
    protected Object readRegularValue(FixedFormatData data) {
        int typeId = data.readInt32();
        
        RecordTypeMapper typeMapper = this.subTypeMappers.get(typeId);
        if (typeMapper == null) {
            throw new InvalidDataException("No type mapper for type id " + typeId + ".");
        }
        
        return typeMapper.readValue(data);
    }
    
    @Override
    public Object handleUnrepresentableValue() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {
        if (this.ownTypeMapper != null) {
            this.ownTypeMapper.writeValue(value, data);
        } else {
            throw new UnsupportedOperationException("Cannot write a value of an abstract type.");
        }
    }
    
}
