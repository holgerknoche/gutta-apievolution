package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class EnumMappingOperation extends UserDefinedTypeMappingOperation<EnumTypeEntry> {
    
    public EnumMappingOperation(EnumTypeEntry typeEntry) {
        super(typeEntry);
    }
        
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        int sourceIndex = source.getInt();
        int targetIndex = this.getTypeEntry().mapIndex(sourceIndex);
        target.putInt(targetIndex);
    }
    
    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleEnumMappingOperation(this);
    }
    
    @Override
    public String toString() {
        return "map enum " + this.getEntryIndex();
    }
    
}
