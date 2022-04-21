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
    public void apply(int baseOffset, ByteBuffer source, ByteBuffer target) {
    	// TODO How should we handle the base offset? Effectively, this is not a loop operation, but a list mapping operation,
    	// as we need to increment the base offset for each entry (based on its source size)
        for(int count = 0; count < this.repetitions; count++) {
            //this.operations.forEach(operation -> operation.apply(context, source, target));
        }   
    }
    
    @Override
    public <R> R accept(ScriptOperationVisitor<R> visitor) {
        return visitor.handleLoopOperation(this);
    }

}
