package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;

class RecordMappingOperation extends UserDefinedTypeMappingOperation<RecordTypeEntry> {
    
    public RecordMappingOperation(RecordTypeEntry typeEntry) {
        super(typeEntry);
    }
    
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        int offset = source.position();
        
        for (FieldMapping fieldMapping : this.getTypeEntry().getFieldMappings()) {
            fieldMapping.apply(offset, source, target);
        }
    }

    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleRecordMappingOperation(this);
    }
    
    @Override
    public int hashCode() {
        return this.getTypeEntry().hashCode();
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(RecordMappingOperation that) {
        return this.getTypeEntry().equals(that.getTypeEntry());
    }
            
    @Override
    public String toString() {
        return "map record " + this.getEntryIndex();
    }

}
