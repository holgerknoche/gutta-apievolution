package gutta.apievolution.fixedformat.objectmapping;

import java.util.Collection;
import java.util.Map;

class PolymorphicRecordTypeMapper implements TypeMapper<Object> {
    
    private static final int DISCRIMINATOR_SIZE = 4;

    private final int maxLength;
    
    private final RecordTypeMapper ownTypeMapper;
    
    private final Map<Integer, RecordTypeMapper> subTypeMappers;
    
    public PolymorphicRecordTypeMapper(RecordTypeMapper ownTypeMapper, Map<Integer, RecordTypeMapper> subTypeMappers) {
        this.ownTypeMapper = ownTypeMapper;
        this.subTypeMappers = subTypeMappers;
        this.maxLength = determineMaxLength(subTypeMappers.values());
    }
    
    private static int determineMaxLength(Collection<RecordTypeMapper> mappers) {
        int maxLength = 0;
        
        for (RecordTypeMapper mapper : mappers) {
            if (mapper.getMaxLength() > maxLength) {
                maxLength = mapper.getMaxLength();
            }
        }
        
        return (maxLength + DISCRIMINATOR_SIZE);
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public int getMaxLength() {
        return this.maxLength;
    }

    @Override()
    public Object readValue(FixedFormatData data) {
        int typeId = data.readInt32();
        
        RecordTypeMapper typeMapper = this.subTypeMappers.get(typeId);
        if (typeMapper == null) {
            throw new InvalidDataException("No type mapper for type id " + typeId + ".");
        }
        
        return typeMapper.readValue(data);
    }

    @Override
    public void writeValue(Object value, FixedFormatData data) {
        if (this.ownTypeMapper != null) {
            this.ownTypeMapper.writeValue(value, data);
        } else {
            throw new UnsupportedOperationException("Cannot write a value of an abstract type.");
        }
    }
    
}
