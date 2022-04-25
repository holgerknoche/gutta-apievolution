package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public class CopyOperation implements ApiMappingOperation {
    
    final int length;
        
    public CopyOperation(int length) {
        this.length = length;
    }
    
    @Override
    public void apply(int offset, ByteBuffer source, ByteBuffer target) {
        byte[] copyBuffer = new byte[this.length];
        
        // Copy data from the given source offset. Starting with Java 13, there is
        // also a built-in function for this        
        source.position(offset);
        source.get(copyBuffer);
        
        target.put(copyBuffer);
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleCopyOperation(this);
    }

}
