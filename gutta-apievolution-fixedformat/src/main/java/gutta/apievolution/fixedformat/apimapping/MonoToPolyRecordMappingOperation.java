package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;

class MonoToPolyRecordMappingOperation extends AbstractPolymorphicRecordMappingOperation {

    private final int targetTypeId;
    
    private final MonomorphicRecordMappingOperation delegate;
    
    public MonoToPolyRecordMappingOperation(RecordTypeEntry targetTypeEntry) {
        this.targetTypeId = targetTypeEntry.getTypeId();
        this.delegate = new MonomorphicRecordMappingOperation(targetTypeEntry);
    }
    
    @Override
    protected int getTargetDataLength() {
        return (this.delegate.getTargetDataLength() + TYPE_ID_SIZE);
    }

    @Override
    protected boolean mayBeUnrepresentable() {
        return false;
    }
    
    public int getTargetTypeId() {
        return this.targetTypeId;
    }
    
    public int getEntryIndex() {
        return this.delegate.getEntryIndex();
    }
    
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        target.putInt(this.targetTypeId);
        this.delegate.mapNonNullValue(source, target);
    }
    
    @Override
    public int hashCode() {
        return (this.targetTypeId + this.delegate.hashCode());
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(MonoToPolyRecordMappingOperation that) {
        return (this.targetTypeId == that.targetTypeId) &&
               this.delegate.equals(that.delegate); 
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleMonoToPolyRecordMappingOperation(this);
    }

}
