package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

import static gutta.apievolution.fixedformat.objectmapping.Flags.IS_ABSENT;
import static gutta.apievolution.fixedformat.objectmapping.Flags.IS_PRESENT;
import static gutta.apievolution.fixedformat.objectmapping.Flags.IS_UNREPRESENTABLE;

abstract class NullableTypeMappingOperation implements ApiMappingOperation {
    
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
            if (!this.mayBeUnrepresentable()) {
                target.put(IS_PRESENT);
            }
            
            this.mapNonNullValue(source, target);
            break;
            
        case IS_UNREPRESENTABLE:
            throw new UnsupportedOperationException("Unrepresentable values cannot be mapped.");
            
        default:
            throw new IllegalStateException("Unsupported flag value " + flags + ".");
        }
        
    }
    
    /**
     * Denotes that this type mapping operation may result in an unrepresentable
     * value. In such cases, the {@link #mapNonNullValue(ByteBuffer, ByteBuffer)}
     * operation must write the value flags itself.
     * 
     * @return {@code True} if this operation may result in an unrepresentable value, {@code false} otherwise
     */
    protected boolean mayBeUnrepresentable() {
        return false;
    }
    
    protected void writeNulls(ByteBuffer target) {
        byte[] paddingData = new byte[this.getTargetDataLength()];
        target.put(paddingData);
    }
    
    protected abstract int getTargetDataLength();
    
    protected abstract void mapNonNullValue(ByteBuffer source, ByteBuffer target);

}
