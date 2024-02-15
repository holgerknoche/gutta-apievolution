package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;

class CopyOperation implements ApiMappingOperation {
    
    private final int length;
        
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
    
    public int getLength() {
        return this.length;
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleCopyOperation(this);
    }

    @Override
    public String toString() {
        return "copy " + this.length;
    }
    
    @Override
    public int hashCode() {
        return this.length;
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(CopyOperation that) {
        return (this.length == that.length);
    }
    
}
