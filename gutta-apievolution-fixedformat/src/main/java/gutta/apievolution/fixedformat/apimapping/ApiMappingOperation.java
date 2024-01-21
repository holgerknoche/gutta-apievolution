package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

interface ApiMappingOperation {
    
    void apply(int sourceOffset, ByteBuffer source, ByteBuffer target);
    
    <R> R accept(ApiMappingOperationVisitor<R> visitor);

}
