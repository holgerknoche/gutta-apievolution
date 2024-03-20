package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.objectmapping.Flags;

import java.nio.ByteBuffer;

class PolyToMonoRecordMappingOperation extends AbstractPolymorphicRecordMappingOperation {

    private final int expectedTypeId;
    
    private final MonomorphicRecordMappingOperation delegate;
    
    public PolyToMonoRecordMappingOperation(int sourceTypeId, RecordTypeEntry targetTypeEntry) {
        this.expectedTypeId = sourceTypeId;
        this.delegate = new MonomorphicRecordMappingOperation(targetTypeEntry);
    }
    
    @Override
    protected int getTargetDataLength() {
        return this.delegate.getTargetDataLength();
    }

    @Override
    protected boolean mayBeUnrepresentable() {
        return true;
    }
    
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        int sourceTypeId = source.getInt();
        if (sourceTypeId != this.expectedTypeId) {
            // If the actual type id does not match the expected one, the value is unrepresentable
            target.put(Flags.IS_UNREPRESENTABLE);
            this.writeNulls(target);
            return;
        }
        
        target.put(Flags.IS_PRESENT);
        this.delegate.mapNonNullValue(source, target);
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handlePolyToMonoRecordMappingOperation(this);
    }

}
