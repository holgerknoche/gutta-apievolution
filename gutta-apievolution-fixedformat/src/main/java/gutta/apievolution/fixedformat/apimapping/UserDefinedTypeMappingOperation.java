package gutta.apievolution.fixedformat.apimapping;

abstract class UserDefinedTypeMappingOperation<T extends TypeEntry> extends NullableTypeMappingOperation {
    
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
    protected int getTargetDataLength() {
        return this.getTypeEntry().getDataSize();
    }

}
