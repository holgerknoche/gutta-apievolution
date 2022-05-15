package gutta.apievolution.fixedformat.apimapping;

public class ApiMappingScriptPrinter {
    
    public String printMappingScript(ApiMappingScript script) {
        StringBuilder scriptBuilder = new StringBuilder();
        
        TypeEntryPrinter typeEntryPrinter = new TypeEntryPrinter(scriptBuilder);
        for (TypeEntry typeEntry : script) {
            typeEntry.accept(typeEntryPrinter);
        }        
        
        return scriptBuilder.toString();
    }
    
    private static class TypeEntryPrinter implements TypeEntryVisitor<Void> {
        
        private final StringBuilder scriptBuilder;
        
        private final MappingOperationPrinter operationPrinter;
        
        public TypeEntryPrinter(StringBuilder scriptBuilder) {
            this.scriptBuilder = scriptBuilder;
            this.operationPrinter = new MappingOperationPrinter(scriptBuilder);
        }
        
        @Override
        public Void handleEnumTypeEntry(EnumTypeEntry enumTypeEntry) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("enum ");
        	builder.append(enumTypeEntry.getTypeId());
        	builder.append(" [");
        	
        	int[] indexMap = enumTypeEntry.getIndexMap();
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
        public Void handleRecordTypeEntry(RecordTypeEntry recordTypeEntry) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("record ");
        	builder.append(recordTypeEntry.getTypeId());
        	builder.append("\n");
        	
        	for (FieldMapping fieldMapping : recordTypeEntry) {
        		builder.append("@");
        		builder.append(fieldMapping.getOffset());
        		builder.append(": ");
        		
        		fieldMapping.getMappingOperation().accept(this.operationPrinter);
        		builder.append("\n");
        	}
        	
        	return null;
        }
        
    }
    
    private static class MappingOperationPrinter implements ApiMappingOperationVisitor<Void> {
        
        private final StringBuilder scriptBuilder;
        
        public MappingOperationPrinter(StringBuilder scriptBuilder) {
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
        	builder.append(enumMappingOperation.getEntryIndex());
        	
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
        	builder.append(recordMappingOperation.getEntryIndex());
        	
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
