package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.List;
import java.util.Objects;

class RecordTypeEntry extends TypeEntry {
    
    private final List<FieldMapping> fieldMappings;

    public RecordTypeEntry(int entryIndex, int typeId, int dataLength, List<FieldMapping> fieldMappings) {
        super(entryIndex, typeId, dataLength);
        
        this.fieldMappings = fieldMappings;
    }
        
    public List<FieldMapping> getFieldMappings() {
        return this.fieldMappings;
    }
    
    @Override
    ApiMappingOperation createMappingOperation() {
        return new MonomorphicRecordMappingOperation(this);
    }
    
    @Override
    <R> R accept(TypeEntryVisitor<R> visitor) {
        return visitor.handleRecordTypeEntry(this);
    }
    
    @Override
    public int hashCode() {
        return this.fieldMappings.hashCode();
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(RecordTypeEntry that) {
        return super.equalsInternal(that) &&
               Objects.equals(this.fieldMappings, that.fieldMappings);
    }
    
}
