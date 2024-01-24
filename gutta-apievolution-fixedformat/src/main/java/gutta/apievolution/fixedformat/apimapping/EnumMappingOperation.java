package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

import static gutta.apievolution.fixedformat.objectmapping.Flags.*;

class EnumMappingOperation extends UserDefinedTypeMappingOperation<EnumTypeEntry> {
    
    public EnumMappingOperation(EnumTypeEntry typeEntry) {
        super(typeEntry);
    }
        
    @Override
    protected boolean mayBeUnrepresentable() {
        return true;
    }
    
    @Override
    protected void mapNonNullValue(ByteBuffer source, ByteBuffer target) {
        int sourceIndex = source.getInt();
        int targetIndex = this.getTypeEntry().mapIndex(sourceIndex);
        
        if (targetIndex >= 0) {
            target.put(IS_PRESENT);
            target.putInt(targetIndex);
        } else {
            target.put(IS_UNREPRESENTABLE);
            target.putInt(0);
        }       
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
