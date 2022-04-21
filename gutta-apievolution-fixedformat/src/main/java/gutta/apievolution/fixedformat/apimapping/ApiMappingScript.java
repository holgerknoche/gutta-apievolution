package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;

public class ApiMappingScript {
    
    private final Map<Integer, MappingProcedure> procedureMap;
    
    public ApiMappingScript(Map<Integer, MappingProcedure> procedureMap) {
        this.procedureMap = procedureMap;
    }
    
    public void apply(Integer typeId, ByteBuffer source, ByteBuffer target) {
        MappingProcedure procedure = this.procedureMap.get(typeId);
        if (procedure == null) {
            throw new IllegalArgumentException("No mapping procedure for type id " + typeId + ".");
        }
                        	
        procedure.apply(0, source, target);
    }
    
    public static class MappingProcedure {
        
        private final List<ScriptOperation> operations; 
        
        public MappingProcedure(List<ScriptOperation> operations) {
            this.operations = operations;
        }
        
        public void apply(int baseOffset, ByteBuffer source, ByteBuffer target) {
            this.operations.forEach(operation -> operation.apply(baseOffset, source, target));
        }
        
    }

}
