package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;

class RecordMappingOperation extends UserDefinedTypeMappingOperation {
    
    public RecordMappingOperation(int entryIndex) {
        super(entryIndex);
    }
    
    @Override
    public void apply(int offset, TypeEntryResolver typeEntryResolver, ByteBuffer source, ByteBuffer target) {
        RecordTypeEntry typeEntry = typeEntryResolver.resolveEntry(this.getEntryIndex());
        
        for (FieldMapping fieldMapping : typeEntry) {
            fieldMapping.apply(offset, typeEntryResolver, source, target);
        }
    }

    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleRecordMappingOperation(this);
    }
            
    @Override
    public String toString() {
        return "map record " + this.getEntryIndex();
    }

}
