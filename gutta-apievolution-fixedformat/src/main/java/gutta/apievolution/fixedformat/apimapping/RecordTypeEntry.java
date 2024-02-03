package gutta.apievolution.fixedformat.apimapping;

import java.util.Iterator;
import java.util.List;

class RecordTypeEntry extends TypeEntry implements Iterable<FieldMapping> {
    
    private final List<FieldMapping> fieldMappings;

    public RecordTypeEntry(int entryIndex, int typeId, int dataLength, List<FieldMapping> fieldMappings) {
        super(entryIndex, typeId, dataLength);
        
        this.fieldMappings = fieldMappings;
    }
    
    @Override
    public Iterator<FieldMapping> iterator() {
        return this.fieldMappings.iterator();
    }
    
    @Override
    ApiMappingOperation createMappingOperation() {
        return new RecordMappingOperation(this);
    }
    
    @Override
    <R> R accept(TypeEntryVisitor<R> visitor) {
        return visitor.handleRecordTypeEntry(this);
    }
    
}
