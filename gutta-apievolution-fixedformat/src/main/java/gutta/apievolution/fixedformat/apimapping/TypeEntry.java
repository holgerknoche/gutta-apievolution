package gutta.apievolution.fixedformat.apimapping;

abstract class TypeEntry {
    
    private final int entryIndex;
    
    private final int typeId;
    
    private final int dataSize;
    
    protected TypeEntry(int entryIndex, int typeId, int dataSize) {
        this.entryIndex = entryIndex;
        this.typeId = typeId;
        this.dataSize = dataSize;
    }
    
    public int getEntryIndex() {
        return this.entryIndex;
    }
    
    public int getTypeId() {
        return this.typeId;
    }
    
    public int getDataSize() {
        return this.dataSize;
    }
    
    abstract ApiMappingOperation createMappingOperation();
    
    abstract <R> R accept(TypeEntryVisitor<R> visitor);

}
