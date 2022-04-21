package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public class CopyOperation implements ScriptOperation {
    
    final int sourceOffset;
    
    final int length;
        
    public CopyOperation(int sourceOffset, int length) {
        this.sourceOffset = sourceOffset;
        this.length = length;
    }
    
    @Override
    public void apply(int baseOffset, ByteBuffer source, ByteBuffer target) {
        byte[] copyBuffer = new byte[this.length];
        
        // Copy data from the given source offset. Starting with Java 13, there is
        // also a built-in function for this        
        source.position(baseOffset + this.sourceOffset);
        source.get(copyBuffer);
        
        target.put(copyBuffer);
    }
    
    @Override
    public <R> R accept(ScriptOperationVisitor<R> visitor) {
        return visitor.handleCopyOperation(this);
    }

}
