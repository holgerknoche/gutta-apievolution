package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class EnumMappingOperation extends UserDefinedTypeMappingOperation {
    
    public EnumMappingOperation(int entryIndex) {
        super(entryIndex);
    }
        
    @Override
    public void apply(int offset, TypeEntryResolver typeEntryResolver, ByteBuffer source, ByteBuffer target) {
        EnumTypeEntry typeEntry = typeEntryResolver.resolveEntry(this.getEntryIndex());
        
        int sourceIndex = source.getInt(offset);
        int targetIndex = typeEntry.mapIndex(sourceIndex);
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
