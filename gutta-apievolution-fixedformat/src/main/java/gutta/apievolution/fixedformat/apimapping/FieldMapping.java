package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

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
    
    public void apply(int baseOffset, TypeEntryResolver typeEntryResolver, ByteBuffer source, ByteBuffer target) {
        int effectiveOffset = (baseOffset + this.offset);
        this.mappingOperation.apply(effectiveOffset, typeEntryResolver, source, target);
    }

}
