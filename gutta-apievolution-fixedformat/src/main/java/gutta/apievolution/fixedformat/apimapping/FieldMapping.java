package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public class FieldMapping {
    
    private final int offset;
    
    private final ApiMappingOperation mappingOperation;
    
    public FieldMapping(int offset, ApiMappingOperation mappingOperation) {
        this.offset = offset;
        this.mappingOperation = mappingOperation;
    }
    
    public void apply(int baseOffset, ByteBuffer source, ByteBuffer target) {
        int effectiveOffset = (baseOffset + this.offset);
        this.mappingOperation.apply(effectiveOffset, source, target);
    }

}
