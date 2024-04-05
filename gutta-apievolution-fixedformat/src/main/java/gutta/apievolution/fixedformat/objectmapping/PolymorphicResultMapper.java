package gutta.apievolution.fixedformat.objectmapping;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

class PolymorphicResultMapper extends AbstractRecordTypeMapper {

    private final Map<Integer, MappingInfo> typeIdToMappingInfo;

    private final int dataLength;

    public PolymorphicResultMapper(Class<?> formalResultType, Map<Integer, RecordTypeMapper> resultTypeMappers,
            Map<Integer, RecordTypeMapper> exceptionTypeMappers) {

        super(formalResultType);
        this.typeIdToMappingInfo = buildMappingInfo(resultTypeMappers, exceptionTypeMappers);

        this.dataLength = determineMaxLength(this.typeIdToMappingInfo.values()) + DISCRIMINATOR_SIZE;
    }

    private static Map<Integer, MappingInfo> buildMappingInfo(Map<Integer, RecordTypeMapper> resultTypeMappers,
            Map<Integer, RecordTypeMapper> exceptionTypeMappers) {
        Map<Integer, MappingInfo> typeIdToMappingInfo = new HashMap<>(resultTypeMappers.size() + exceptionTypeMappers.size());

        resultTypeMappers.forEach((typeId, typeMapper) -> typeIdToMappingInfo.put(typeId, new MappingInfo(typeMapper, false)));
        exceptionTypeMappers.forEach((typeId, typeMapper) -> typeIdToMappingInfo.put(typeId, new MappingInfo(typeMapper, true)));

        return typeIdToMappingInfo;

    }

    private static int determineMaxLength(Collection<MappingInfo> mappingInfoEntries) {
        int maxLength = 0;

        for (MappingInfo mappingInfo : mappingInfoEntries) {
            int length = mappingInfo.getTypeMapper().getDataLength();

            if (length > maxLength) {
                maxLength = length;
            }
        }

        return maxLength;
    }

    @Override
    protected int getDataLength() {
        return this.dataLength;
    }

    @Override
    protected Object readRegularValue(FixedFormatData data) {
        int typeId = data.readInt32();

        MappingInfo mappingInfo = this.getMappingInfoFor(typeId);
        Object readValue = mappingInfo.getTypeMapper().readRegularValue(data);

        if (mappingInfo.isException()) {
            return ValueOrException.forException(readValue);
        } else {
            return ValueOrException.forValue(readValue);
        }
    }

    private MappingInfo getMappingInfoFor(int typeId) {
        MappingInfo mappingInfo = this.typeIdToMappingInfo.get(typeId);
        if (mappingInfo == null) {
            throw new InvalidDataException("No mapping info for type id " + typeId + ".");
        }

        return mappingInfo;
    }

    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {
        // Write the type id
        int typeId = this.determineTypeIdFor(value);
        data.writeInt32(typeId);

        // Use the type mapper to write the value
        MappingInfo mappingInfo = this.getMappingInfoFor(typeId);
        mappingInfo.getTypeMapper().writeRegularValue(value, data);
    }

    private static class MappingInfo {

        private final RecordTypeMapper typeMapper;

        private final boolean exception;

        public MappingInfo(RecordTypeMapper typeMapper, boolean exception) {
            this.typeMapper = typeMapper;
            this.exception = exception;
        }

        public RecordTypeMapper getTypeMapper() {
            return this.typeMapper;
        }

        public boolean isException() {
            return this.exception;
        }

    }

}
