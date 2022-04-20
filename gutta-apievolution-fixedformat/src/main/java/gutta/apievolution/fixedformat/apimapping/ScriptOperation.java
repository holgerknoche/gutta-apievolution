package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

public interface ScriptOperation {
    
    void apply(ByteBuffer source, ByteBuffer target);
    
    <R> R accept(ScriptOperationVisitor<R> visitor);

}
