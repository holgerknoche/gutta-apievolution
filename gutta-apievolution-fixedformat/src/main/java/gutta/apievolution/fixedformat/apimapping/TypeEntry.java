package gutta.apievolution.fixedformat.apimapping;

abstract class TypeEntry {
    
    private final int entryIndex;
    
    private final int typeId;
    
    protected TypeEntry(int entryIndex, int typeId) {
        this.entryIndex = entryIndex;
        this.typeId = typeId;
    }
    
    public int getEntryIndex() {
        return this.entryIndex;
    }
    
    public int getTypeId() {
        return this.typeId;
    }
    
    abstract ApiMappingOperation createMappingOperation();
    
    abstract <R> R accept(TypeEntryVisitor<R> visitor);

}
