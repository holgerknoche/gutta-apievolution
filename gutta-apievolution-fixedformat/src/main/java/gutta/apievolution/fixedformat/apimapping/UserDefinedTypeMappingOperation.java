package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

import static gutta.apievolution.fixedformat.objectmapping.Flags.*;

abstract class UserDefinedTypeMappingOperation<T extends TypeEntry> implements ApiMappingOperation {
    
    private final T typeEntry;
    
    protected UserDefinedTypeMappingOperation(T typeEntry) {
        this.typeEntry = typeEntry;
    }
   
    public int getEntryIndex() {
        return this.typeEntry.getEntryIndex();
    }
    
    protected T getTypeEntry() {
        return this.typeEntry;
    }
    
    @Override
    public final void apply(int sourceOffset, ByteBuffer source, ByteBuffer target) {
        source.position(sourceOffset);
        byte flags = source.get();
        
        switch (flags) {
        case IS_ABSENT:
            target.put(IS_ABSENT);
            this.writeNulls(target);
            break;
            
        case IS_PRESENT:
            target.put(IS_PRESENT);
            this.mapNonNullValue(source, target);
            break;
            
        case IS_UNREPRESENTABLE:
            throw new UnsupportedOperationException("Unrepresentable values cannot be mapped.");
            
        default:
            throw new IllegalStateException("Unsupported flag value " + flags + ".");
        }
        
    }
    
    private void writeNulls(ByteBuffer target) {
        byte[] paddingData = new byte[this.getTypeEntry().getDataSize()];
        target.put(paddingData);
    }
    
    protected abstract void mapNonNullValue(ByteBuffer source, ByteBuffer target);

}
