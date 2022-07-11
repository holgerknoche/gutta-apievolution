package gutta.apievolution.fixedformat.apimapping;

import java.util.Arrays;

class EnumTypeEntry extends TypeEntry {
    
    private final int[] indexMap;
    
    public EnumTypeEntry(int entryIndex, int typeId, int[] indexMap) {
        super(entryIndex, typeId);
        
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
        return new EnumMappingOperation(this.getEntryIndex());
    }

    @Override
    <R> R accept(TypeEntryVisitor<R> visitor) {
        return visitor.handleEnumTypeEntry(this);
    }
    
}
