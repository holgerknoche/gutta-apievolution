package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.List;

public class LoopOperation implements ScriptOperation {
    
    final int repetitions;
    
    final List<ScriptOperation> operations;
    
    public LoopOperation(int repetitions, List<ScriptOperation> operations) {
        this.repetitions = repetitions;
        this.operations = operations;
    }
    
    @Override
    public void apply(ByteBuffer source, ByteBuffer target) {
        for(int count = 0; count < this.repetitions; count++) {
            this.operations.forEach(operation -> operation.apply(source, target));
        }   
    }
    
    @Override
    public <R> R accept(ScriptOperationVisitor<R> visitor) {
        return visitor.handleLoopOperation(this);
    }

}
