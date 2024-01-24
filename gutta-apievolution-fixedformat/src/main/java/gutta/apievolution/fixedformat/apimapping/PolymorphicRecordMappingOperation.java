package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.objectmapping.Flags;

import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

class PolymorphicRecordMappingOperation extends NullableTypeMappingOperation {

    private static final int TYPE_ID_SIZE = 4;
    
    private final Map<Integer, PolymorphicRecordMapping> idToRecordMapping;
    
    private final int dataLength;
    
    public PolymorphicRecordMappingOperation(Map<Integer, PolymorphicRecordMapping> idToRecordMapping) {
        this.idToRecordMapping = idToRecordMapping;
        this.dataLength = determineMaxDataLength(idToRecordMapping.values());
    }
    
    private static int determineMaxDataLength(Collection<PolymorphicRecordMapping> mappings) {
        int maxDataLength = 0;
        
        for (PolymorphicRecordMapping mapping : mappings) {
            int currentDataLength = mapping.getTypeEntry().getDataSize();
            if (currentDataLength > maxDataLength) {
                maxDataLength = currentDataLength;
            }
        }
        
        return (maxDataLength + TYPE_ID_SIZE);
    }
    
    @Override
    protected int getTargetDataLength() {
        return this.dataLength;
    }
    
    @Override
    protected boolean mayBeUnrepresentable() {
        return true;
    }
    
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        int sourceTypeId = source.getInt();
        PolymorphicRecordMapping recordMapping = this.idToRecordMapping.get(sourceTypeId);
        
        if (recordMapping == null) {
            // If the type id does not exist, the value is unrepresentable.
            target.put(Flags.IS_UNREPRESENTABLE);
            this.writeNulls(target);
            return;
        }
        
        target.put(Flags.IS_PRESENT);
        target.putInt(recordMapping.getTargetTypeId());
        
        RecordMappingOperation actualMappingOperation = new RecordMappingOperation(recordMapping.getTypeEntry());
        actualMappingOperation.mapNonNullValue(source, target);
    }

    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handlePolymorphicRecordMappingOperation(this);
    }
    
    Collection<PolymorphicRecordMapping> getRecordMappings() {
        return Collections.unmodifiableCollection(this.idToRecordMapping.values());
    }
    
    static class PolymorphicRecordMapping {
        
        private final int sourceTypeId;
        
        private final int targetTypeId;
        
        private final RecordTypeEntry typeEntry;
        
        public PolymorphicRecordMapping(int sourceTypeId, int targetTypeId, RecordTypeEntry typeEntry) {
            this.sourceTypeId = sourceTypeId;
            this.targetTypeId = targetTypeId;
            this.typeEntry = typeEntry;
        }
        
        public int getSourceTypeId() {
            return this.sourceTypeId;
        }
        
        public int getTargetTypeId() {
            return this.targetTypeId;
        }
        
        public RecordTypeEntry getTypeEntry() {
            return this.typeEntry;
        }
        
    }    

}
