package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import gutta.apievolution.core.apimodel.UserDefinedType;

public class ApiMappingScript implements Iterable<UserDefinedTypeMappingOperation> {
    
	private final List<UserDefinedTypeMappingOperation> operations;
	
    private final Map<Integer, UserDefinedTypeMappingOperation> typeToOperation;
    
    public ApiMappingScript(List<UserDefinedTypeMappingOperation> operations) {
    	this.operations = operations;
        this.typeToOperation = operations.stream().collect(Collectors.toMap(op -> op.getTypeId(), Function.identity()));
    }
    
    public void mapType(UserDefinedType<?> type, ByteBuffer source, ByteBuffer target) {
    	this.mapType(type.getTypeId(), source, target);
    }
    
    public void mapType(Integer typeId, ByteBuffer source, ByteBuffer target) {
    	UserDefinedTypeMappingOperation mappingOperation = this.typeToOperation.get(typeId);
        if (mappingOperation == null) {
            throw new IllegalArgumentException("No mapping for type id " + typeId + ".");
        }
                        	
        mappingOperation.apply(0, source, target);
    }
    
    @Override
    public Iterator<UserDefinedTypeMappingOperation> iterator() {
        return this.operations.iterator();
    }
    
    public int size() {
    	return this.operations.size();
    }

}
