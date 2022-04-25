package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public class EnumMappingOperation implements ApiMappingOperation {

    final int[] indexMap;
    
    public EnumMappingOperation(int[] indexMap) {
        this.indexMap = indexMap;
    }
        
    @Override
    public void apply(int offset, ByteBuffer source, ByteBuffer target) {
        int sourceIndex = source.getInt(offset);
        int targetIndex = this.indexMap[sourceIndex];
        target.putInt(targetIndex);
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleEnumMappingOperation(this);
    }
    
}
