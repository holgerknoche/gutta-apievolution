package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.resolution.DefinitionResolution;

public class ApiMappingScriptGenerator {
    
    public ApiMappingScript generateMappingScript(DefinitionResolution resolution, MappingDirection direction) {
        
        // TODO
        return null;
    }
  
    private static class TypeInfo {
        
        private final int typeId;
        
        public TypeInfo(int typeId) {
            this.typeId = typeId;
        }
        
    }
    
    private static class FieldInfo {
        
        private final int offset;
        
        private final int size;
        
        public FieldInfo(int offset, int size) {
            this.offset = offset;
            this.size = size;
        }
        
    }
    
    public enum MappingDirection {
        CONSUMER_TO_PRODUCER,
        PRODUCER_TO_CONSUMER
    }

}
