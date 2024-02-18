package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Arrays;

class EnumTypeEntry extends TypeEntry {
    
    private final int[] indexMap;
    
    public EnumTypeEntry(int entryIndex, int typeId, int[] indexMap) {
        super(entryIndex, typeId, 4);
        
        this.indexMap = indexMap;
    }
    
    int mapIndex(int sourceIndex) {
        return this.indexMap[sourceIndex];
    }
    
    int[] getIndexMap() {
        return Arrays.copyOf(this.indexMap, this.indexMap.length);
    }
    
    @Override
    ApiMappingOperation createMappingOperation() {
        return new EnumMappingOperation(this);
    }

    @Override
    <R> R accept(TypeEntryVisitor<R> visitor) {
        return visitor.handleEnumTypeEntry(this);
    }
    
    @Override
    public int hashCode() {
        return this.indexMap.hashCode();
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(EnumTypeEntry that) {
        return super.equalsInternal(that) &&
               Arrays.equals(this.indexMap, that.indexMap);
    }
    
}
