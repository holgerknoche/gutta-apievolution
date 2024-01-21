package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class ListMappingOperation extends NullableTypeMappingOperation {
    
    final int maxElements;
    
    final int sourceElementSize;
    
    final int targetElementSize;
    
    final int targetDataLength;
    
    final ApiMappingOperation elementMappingOperation;
    
    public ListMappingOperation(int maxElements, int sourceElementSize, int targetElementSize,
            ApiMappingOperation elementMappingOperation) {
        this.maxElements = maxElements;
        this.sourceElementSize = sourceElementSize;
        this.targetElementSize = targetElementSize;
        this.targetDataLength = (maxElements * targetElementSize);
        this.elementMappingOperation = elementMappingOperation;
    }
    
    @Override
    protected int getTargetDataLength() {
        return this.targetDataLength;
    }
    
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        // Read and transfer the actual number of arguments
        int actualElements = source.getInt();
        if (actualElements > this.maxElements) {
            throw new IllegalStateException("Too many elements (" + actualElements + ") at offset " + source.position() + 
                    ".");
        }
        
        target.putInt(actualElements);
        
        // Map the elements
        int currentOffset = source.position();
        for (int elementIndex = 0; elementIndex < actualElements; elementIndex++) {
            this.elementMappingOperation.apply(currentOffset, source, target);
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
