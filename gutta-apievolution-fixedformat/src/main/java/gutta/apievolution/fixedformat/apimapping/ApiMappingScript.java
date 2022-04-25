package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.Map;

public class ApiMappingScript {
    
    private final Map<Integer, RecordMappingOperation> recordMap;
    
    public ApiMappingScript(Map<Integer, RecordMappingOperation> recordMap) {
        this.recordMap = recordMap;
    }
    
    public void mapRecord(Integer typeId, ByteBuffer source, ByteBuffer target) {
        RecordMappingOperation mappingOperation = this.recordMap.get(typeId);
        if (mappingOperation == null) {
            throw new IllegalArgumentException("No mapping for type id " + typeId + ".");
        }
                        	
        mappingOperation.apply(0, source, target);
    }

}
