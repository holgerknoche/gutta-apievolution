package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import gutta.apievolution.core.apimodel.UserDefinedType;

public class ApiMappingScript implements Iterable<TypeEntry> {
    
	private final List<TypeEntry> typeEntries;
	
    private final Map<Integer, TypeEntry> typeToEntry;
    
    ApiMappingScript(List<TypeEntry> typeEntries) {
    	this.typeEntries = typeEntries;
        this.typeToEntry = typeEntries.stream().collect(Collectors.toMap(op -> op.getTypeId(), Function.identity()));
    }
    
    public void mapType(UserDefinedType<?> type, ByteBuffer source, ByteBuffer target) {
    	this.mapType(type.getTypeId(), source, target);
    }
    
    public void mapType(Integer typeId, ByteBuffer source, ByteBuffer target) {
    	TypeEntry typeEntry = this.typeToEntry.get(typeId);
        if (typeEntry == null) {
            throw new IllegalArgumentException("No entry for type id " + typeId + ".");
        }
                        	
        ApiMappingOperation mappingOperation = typeEntry.createMappingOperation();
        mappingOperation.apply(0, this::resolveTypeEntry, source, target);
    }
    
    @SuppressWarnings("unchecked")
    private <T extends TypeEntry> T resolveTypeEntry(int entryIndex) {
        return (T) this.typeEntries.get(entryIndex);
    }
    
    @Override
    public Iterator<TypeEntry> iterator() {
        return this.typeEntries.iterator();
    }
    
    int size() {
    	return this.typeEntries.size();
    }

}
