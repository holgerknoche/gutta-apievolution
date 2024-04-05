package gutta.apievolution.fixedformat.objectmapping;

import java.util.Collection;
import java.util.Map;

class PolymorphicRecordTypeMapper extends AbstractRecordTypeMapper {
    
    private final int dataLength;

    private final Map<Integer, RecordTypeMapper> subTypeMappers;

    public PolymorphicRecordTypeMapper(Class<?> formalResultType, Map<Integer, RecordTypeMapper> subTypeMappers) {
        super(formalResultType);
        
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
    protected int getDataLength() {
        return this.dataLength;
    }

    private RecordTypeMapper getTypeMapperFor(int typeId) {
        RecordTypeMapper typeMapper = this.subTypeMappers.get(typeId);

        if (typeMapper == null) {
            throw new InvalidDataException("No type mapper for type id " + typeId + ".");
        }

        return typeMapper;
    }

    @Override()
    protected Object readRegularValue(FixedFormatData data) {
        int typeId = data.readInt32();

        RecordTypeMapper typeMapper = this.getTypeMapperFor(typeId);
        // Null checks have already been performed, therefore invoke readRegularValue
        return typeMapper.readRegularValue(data);
    }

    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {
        int typeId = this.determineTypeIdFor(value);
        data.writeInt32(typeId);

        RecordTypeMapper typeMapper = this.getTypeMapperFor(typeId);
        // Non-null flag has already been written
        typeMapper.writeRegularValue(value, data);
    }
    
}
