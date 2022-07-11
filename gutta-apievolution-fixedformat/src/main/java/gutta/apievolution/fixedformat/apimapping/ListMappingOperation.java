package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class ListMappingOperation implements ApiMappingOperation {
    
    final int maxElements;
    
    final int sourceElementSize;
    
    final int targetElementSize;
    
    final ApiMappingOperation elementMappingOperation;
    
    public ListMappingOperation(int maxElements, int sourceElementSize, int targetElementSize,
            ApiMappingOperation elementMappingOperation) {
        this.maxElements = maxElements;
        this.sourceElementSize = sourceElementSize;
        this.targetElementSize = targetElementSize;
        this.elementMappingOperation = elementMappingOperation;
    }
    
    @Override
    public void apply(int sourceOffset, TypeEntryResolver typeEntryResolver, ByteBuffer source, ByteBuffer target) {
        source.position(sourceOffset);
        // Read and transfer the actual number of arguments
        int actualElements = source.getInt();
        if (actualElements > this.maxElements) {
            throw new IllegalStateException("Too many elements (" + actualElements + ") at offset " + sourceOffset + 
                    ".");
        }
        
        target.putInt(actualElements);
        
        // Map the elements
        int currentOffset = (sourceOffset + 4);
        for (int elementIndex = 0; elementIndex < actualElements; elementIndex++) {
            this.elementMappingOperation.apply(currentOffset, typeEntryResolver, source, target);
            currentOffset += this.sourceElementSize;
        }
        
        // Skip unused bytes in the target buffer
        int unusedElements = (this.maxElements - actualElements);
        int targetOffset = target.position();
        target.position(targetOffset + (unusedElements * targetElementSize));
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleListMappingOperation(this);
    }
    
    @Override
    public String toString() {
        return "list " + this.maxElements + ", " + this.sourceElementSize + ", " + this.targetElementSize;
    }

}
