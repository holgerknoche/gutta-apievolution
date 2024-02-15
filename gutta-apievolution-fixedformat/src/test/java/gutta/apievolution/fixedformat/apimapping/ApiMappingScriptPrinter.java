package gutta.apievolution.fixedformat.apimapping;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;

public class ApiMappingScriptPrinter {
    
    public String printMappingScript(ApiMappingScript script) {
        StringBuilder scriptBuilder = new StringBuilder();
        
        TypeEntryPrinter typeEntryPrinter = new TypeEntryPrinter(scriptBuilder);
        int typeIndex = 0;
        for (TypeEntry typeEntry : script.getTypeEntries()) {
            scriptBuilder.append("type index " + typeIndex + ":\n");
            
            typeEntry.accept(typeEntryPrinter);
            
            typeIndex++;
        }
        
        for (OperationEntry operationEntry : script.getOperationEntries()) {
            this.printOperationEntry(operationEntry, scriptBuilder);
        }
        
        return scriptBuilder.toString();
    }
    
    private void printOperationEntry(OperationEntry entry, StringBuilder builder) {
        builder.append("operation ");
        builder.append(entry.getName());
        builder.append(" param: ");
        builder.append(entry.getParameterMappingOperation());
        builder.append(" result: ");
        builder.append(entry.getResultMappingOperation());
        builder.append("\n");        
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
        	
        	for (FieldMapping fieldMapping : recordTypeEntry.getFieldMappings()) {
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
        	builder.append(copyOperation.getLength());
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
        public Void handlePolymorphicRecordMappingOperation(PolymorphicRecordMappingOperation polymorphicRecordMappingOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("map poly record ");
        	
        	List<PolymorphicRecordMapping> orderedMappings = new ArrayList<>(polymorphicRecordMappingOperation.getRecordMappings());
        	Iterator<PolymorphicRecordMapping> mappings = orderedMappings.iterator();
        	while (mappings.hasNext()) {
        		PolymorphicRecordMapping mapping = mappings.next();
        		
        		builder.append(mapping.getSourceTypeId());
        		builder.append("->(");
        		builder.append(mapping.getTargetTypeId());
        		builder.append("@");
        		builder.append(mapping.getTypeEntry().getEntryIndex());
        		builder.append(")");
        		
        		if (mappings.hasNext()) {
        			builder.append(", ");
        		}
        	}        	
        	
        	return null;
        }
        
        @Override
        public Void handleSkipOperation(SkipOperation skipOperation) {
        	StringBuilder builder = this.scriptBuilder;
        	
        	builder.append("skip ");
        	builder.append(skipOperation.getAmount());
        	builder.append(" bytes");
        	
        	return null;
        }
        
    }
    
}
