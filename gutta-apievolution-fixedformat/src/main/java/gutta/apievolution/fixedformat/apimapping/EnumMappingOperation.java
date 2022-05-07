package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class EnumMappingOperation extends UserDefinedTypeMappingOperation {

    final int[] indexMap;
    
    public EnumMappingOperation(int typeId, int[] indexMap) {
    	super(typeId);
    	
        this.indexMap = indexMap;
    }
        
    @Override
    public void apply(int offset, ByteBuffer source, ByteBuffer target) {
        int sourceIndex = source.getInt(offset);
        int targetIndex = this.indexMap[sourceIndex];
        target.putInt(targetIndex);
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleEnumMappingOperation(this);
    }
    
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        
        builder.append("map enum ");
        builder.append(this.getTypeId());
        builder.append(": [");
        
        int maxIndex = (this.indexMap.length - 1);
        for (int sourceIndex = 0; sourceIndex <= maxIndex; sourceIndex++) {
            builder.append(sourceIndex);
            builder.append(" -> ");
            builder.append(this.indexMap[sourceIndex]);
            
            if (sourceIndex < maxIndex) {
                builder.append(", ");
            }
        }
        
        builder.append("]");
        
        return builder.toString();
    }
    
}
