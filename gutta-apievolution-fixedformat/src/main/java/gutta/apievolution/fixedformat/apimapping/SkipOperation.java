package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;

class SkipOperation implements ApiMappingOperation {
    
    private final int amount;
    
    public SkipOperation(int amount) {
        this.amount = amount;
    }
    
    @Override
    public void apply(int baseOffsetyteBuffer, ByteBuffer source, ByteBuffer target) {
        int currentPosition = target.position();
        target.position(currentPosition + this.amount);
    }
    
    public int getAmount() {
        return this.amount;
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleSkipOperation(this);
    }
    
    @Override
    public int hashCode() {
        return this.amount;
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(SkipOperation that) {
        return (this.amount == that.amount);
    }
    
    @Override
    public String toString() {
        return "skip " + this.amount;
    }

}
