package gutta.apievolution.fixedformat.apimapping;

public class ApiMappingScriptPrinter {
    
    public String printMappingScript(ApiMappingScript script) {
        StringBuilder scriptBuilder = new StringBuilder();
        
        int index = 0;
        Level1Printer printer = new Level1Printer(scriptBuilder);
        
        for (UserDefinedTypeMappingOperation operation : script) {
            scriptBuilder.append("entry ");
            scriptBuilder.append(index);
            scriptBuilder.append("\n");
            
            operation.accept(printer);            
            index++;
        }
        
        return scriptBuilder.toString();
    }

    private static class Level1Printer implements ApiMappingOperationVisitor<Void> {
        
        private final StringBuilder scriptBuilder;
        
        private final Level2Printer subPrinter;
        
        public Level1Printer(StringBuilder scriptBuilder) {
            this.scriptBuilder = scriptBuilder;
            this.subPrinter = new Level2Printer(scriptBuilder);
        }
        
        @Override
        public Void handleEnumMappingOperation(EnumMappingOperation enumMappingOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("enum ");
        	builder.append(enumMappingOperation.getTypeId());
        	builder.append(" [");
        	
        	int[] indexMap = enumMappingOperation.indexMap;
        	int maxIndex = (indexMap.length - 1);
        	for (int sourceIndex = 0; sourceIndex <= maxIndex; sourceIndex++) {
        		int targetIndex = indexMap[sourceIndex];
        		
        		builder.append(sourceIndex);
        		builder.append(" -> ");
        		builder.append(targetIndex);
        		
        		if (sourceIndex < maxIndex) {
        			builder.append(", ");
        		}
        	}
        	
        	builder.append("]\n");
        	
        	return null;
        }
        
        @Override
        public Void handleRecordMappingOperation(RecordMappingOperation recordMappingOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("record ");
        	builder.append(recordMappingOperation.getTypeId());
        	builder.append("\n");
        	
        	for (FieldMapping fieldMapping : recordMappingOperation) {
        		fieldMapping.getMappingOperation().accept(this.subPrinter);
        		builder.append("\n");
        	}
        	
        	return null;
        }
        
    }
    
    private static class Level2Printer implements ApiMappingOperationVisitor<Void> {
        
        private final StringBuilder scriptBuilder;
        
        public Level2Printer(StringBuilder scriptBuilder) {
            this.scriptBuilder = scriptBuilder;
        }
        
        @Override
        public Void handleCopyOperation(CopyOperation copyOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("copy ");
        	builder.append(copyOperation.length);
        	builder.append(" bytes");
        	
        	return null;
        }
        
        @Override
        public Void handleEnumMappingOperation(EnumMappingOperation enumMappingOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("map enum ");
        	builder.append(enumMappingOperation.getTypeId());
        	
        	return null;
        }
        
        @Override
        public Void handleListMappingOperation(ListMappingOperation listMappingOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("map list ");
        	builder.append(listMappingOperation.maxElements);
        	builder.append(" elements, source size=");
        	builder.append(listMappingOperation.sourceElementSize);
        	builder.append(", target size=");
        	builder.append(listMappingOperation.targetElementSize);
        	builder.append(", element mapping=");
        	
        	listMappingOperation.elementMappingOperation.accept(this);
        	
        	return null;
        }
        
        @Override
        public Void handleRecordMappingOperation(RecordMappingOperation recordMappingOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("map record ");
        	builder.append(recordMappingOperation.getTypeId());
        	
        	return null;
        }
        
        @Override
        public Void handleSkipOperation(SkipOperation skipOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("skip ");
        	builder.append(skipOperation.amount);
        	builder.append(" bytes");
        	
        	return null;
        }
        
    }
    
}
