package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;
import java.util.Objects;

class FieldMapping {
    
    private final int offset;
    
    private final ApiMappingOperation mappingOperation;
    
    public FieldMapping(int offset, ApiMappingOperation mappingOperation) {
        this.offset = offset;
        this.mappingOperation = mappingOperation;
    }
    
    public int getOffset() {
        return this.offset;
    }
    
    public ApiMappingOperation getMappingOperation() {
        return this.mappingOperation;
    }
    
    public void apply(int baseOffset, ByteBuffer source, ByteBuffer target) {
        int effectiveOffset = (baseOffset + this.offset);
        this.mappingOperation.apply(effectiveOffset, source, target);
    }
    
    @Override
    public int hashCode() {
        return this.offset;
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(FieldMapping that) {
        return (this.offset == that.offset) &&
               Objects.equals(this.mappingOperation, that.mappingOperation);
    }

}
