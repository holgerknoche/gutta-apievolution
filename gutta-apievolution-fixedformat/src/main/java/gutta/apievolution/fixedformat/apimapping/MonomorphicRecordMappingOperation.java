package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;

class MonomorphicRecordMappingOperation extends UserDefinedTypeMappingOperation<RecordTypeEntry> {
    
    public MonomorphicRecordMappingOperation(RecordTypeEntry typeEntry) {
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
        return visitor.handleMonomorphicRecordMappingOperation(this);
    }
    
    @Override
    public int hashCode() {
        return this.getTypeEntry().hashCode();
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(MonomorphicRecordMappingOperation that) {
        return this.getTypeEntry().equals(that.getTypeEntry());
    }
            
    @Override
    public String toString() {
        return "map monomorphic record " + this.getEntryIndex();
    }

}
