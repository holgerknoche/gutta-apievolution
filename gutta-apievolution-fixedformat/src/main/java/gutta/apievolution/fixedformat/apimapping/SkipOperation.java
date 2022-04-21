package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public class SkipOperation implements ScriptOperation {
    
    final int amount;
    
    public SkipOperation(int amount) {
        this.amount = amount;
    }
    
    @Override
    public void apply(int baseOffset, ByteBuffer source, ByteBuffer target) {
        int currentPosition = target.position();
        target.position(currentPosition + this.amount);
    }
    
    @Override
    public <R> R accept(ScriptOperationVisitor<R> visitor) {
        return visitor.handleSkipOperation(this);
    }

}
