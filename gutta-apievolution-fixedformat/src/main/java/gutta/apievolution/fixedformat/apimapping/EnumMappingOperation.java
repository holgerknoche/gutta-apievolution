package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public class EnumMappingOperation implements ScriptOperation {

    final int sourceOffset;
    
    final int[] indexMap;
    
    public EnumMappingOperation(int sourceOffset, int[] indexMap) {
        this.sourceOffset = sourceOffset;
        this.indexMap = indexMap;
    }
    
    // TODO Offsets are relative to the current record
    
    @Override
    public void apply(ByteBuffer source, ByteBuffer target) {
        int sourceIndex = source.getInt(this.sourceOffset);
        int targetIndex = this.indexMap[sourceIndex];
        target.putInt(targetIndex);
    }
    
    @Override
    public <R> R accept(ScriptOperationVisitor<R> visitor) {
        return visitor.handleEnumMappingOperation(this);
    }
    
}
