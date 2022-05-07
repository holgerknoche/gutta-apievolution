package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.List;

class RecordMappingOperation extends UserDefinedTypeMappingOperation {

    private final List<FieldMapping> fieldMappings;
    
    public RecordMappingOperation(int typeId, List<FieldMapping> fieldMappings) {
    	super(typeId);
    	
        this.fieldMappings = fieldMappings;
    }
    
    @Override
    public void apply(int offset, ByteBuffer source, ByteBuffer target) {
        this.fieldMappings.forEach(operation -> operation.apply(offset, source, target));
    }

    @Override
    public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
        return visitor.handleRecordMappingOperation(this);
    }
    
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        
        builder.append("map record ");
        builder.append(this.getTypeId());
        builder.append("\n");
        
        Iterator<FieldMapping> mappings = this.fieldMappings.iterator();
        while (mappings.hasNext()) {
            FieldMapping fieldMapping = mappings.next();
            
            builder.append("source offset ");
            builder.append(fieldMapping.getOffset());
            builder.append(" -> ");
            
            ApiMappingOperation mappingOperation = fieldMapping.getMappingOperation();
            if (mappingOperation instanceof RecordMappingOperation) {
                builder.append("map record ");
                builder.append(((RecordMappingOperation) mappingOperation).getTypeId());
            } else if (mappingOperation instanceof EnumMappingOperation) {
                builder.append("map enum ");
                builder.append(((EnumMappingOperation) mappingOperation).getTypeId());
            } else {
                builder.append(mappingOperation.toString());
            }
            
            if (mappings.hasNext()) {
                builder.append("\n");
            }
        }
        
        return builder.toString();
    }

}
