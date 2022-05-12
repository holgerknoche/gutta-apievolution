package gutta.apievolution.fixedformat.apimapping;

public class EnumTypeEntry extends TypeEntry {
    
    final int[] indexMap;
    
    public EnumTypeEntry(int entryIndex, int typeId, int[] indexMap) {
        super(entryIndex, typeId);
        
        this.indexMap = indexMap;
    }
    
    int mapIndex(int sourceIndex) {
        return this.indexMap[sourceIndex];
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
