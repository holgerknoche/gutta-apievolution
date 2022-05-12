package gutta.apievolution.fixedformat.apimapping;

import java.util.Iterator;
import java.util.List;

public class RecordTypeEntry extends TypeEntry implements Iterable<FieldMapping> {
    
    private final List<FieldMapping> fieldMappings;

    public RecordTypeEntry(int entryIndex, int typeId, List<FieldMapping> fieldMappings) {
        super(entryIndex, typeId);
        
        this.fieldMappings = fieldMappings;
    }
    
    @Override
    public Iterator<FieldMapping> iterator() {
        return this.fieldMappings.iterator();
    }
    
    @Override
    ApiMappingOperation createMappingOperation() {
        return new RecordMappingOperation(this.getEntryIndex());
    }
    
    @Override
    <R> R accept(TypeEntryVisitor<R> visitor) {
        return visitor.handleRecordTypeEntry(this);
    }
    
}
