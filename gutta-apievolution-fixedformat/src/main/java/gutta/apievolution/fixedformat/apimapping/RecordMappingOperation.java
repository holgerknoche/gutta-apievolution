package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.List;

public class RecordMappingOperation extends UserDefinedTypeMappingOperation {

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

}
