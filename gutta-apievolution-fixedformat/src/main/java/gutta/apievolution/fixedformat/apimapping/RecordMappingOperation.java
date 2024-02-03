package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class RecordMappingOperation extends UserDefinedTypeMappingOperation<RecordTypeEntry> {
    
    public RecordMappingOperation(RecordTypeEntry typeEntry) {
        super(typeEntry);
    }
    
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        int offset = source.position();
        
        for (FieldMapping fieldMapping : this.getTypeEntry()) {
            fieldMapping.apply(offset, source, target);
        }
    }

    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleRecordMappingOperation(this);
    }
            
    @Override
    public String toString() {
        return "map record " + this.getEntryIndex();
    }

}
