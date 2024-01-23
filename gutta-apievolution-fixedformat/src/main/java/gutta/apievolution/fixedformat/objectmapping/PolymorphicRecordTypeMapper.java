package gutta.apievolution.fixedformat.objectmapping;

import java.util.Collection;
import java.util.Map;

class PolymorphicRecordTypeMapper extends TypeMapper<Object> {
    
    private static final int DISCRIMINATOR_SIZE = 4;

    private final int dataLength;
        
    private final Map<Integer, RecordTypeMapper> subTypeMappers;
    
    public PolymorphicRecordTypeMapper(Map<Integer, RecordTypeMapper> subTypeMappers) {
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
    public Object handleUnrepresentableValue() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {
    	TypeId typeIdAnnotation = value.getClass().getAnnotation(TypeId.class);
    	if (typeIdAnnotation == null) {
    		throw new InvalidRepresentationElementException("Missing type id on type " + value.getClass() + ".");
    	}
    	
    	int typeId = typeIdAnnotation.value();
    	data.writeInt32(typeId);

    	RecordTypeMapper typeMapper = this.getTypeMapperFor(typeId);
    	// Non-null flag has already been written
    	typeMapper.writeRegularValue(value, data);
    }
    
}
