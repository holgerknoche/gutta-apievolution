package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * This class represents an API mapping script for converting a fixed-format representation of an API into another.
 */
public class ApiMappingScript implements Iterable<TypeEntry> {    
    
    private final List<TypeEntry> typeEntries;
    
    private final List<OperationEntry> operationEntries;    
        
    private final Map<String, OperationEntry> nameToOperation;

    ApiMappingScript(List<TypeEntry> typeEntries, List<OperationEntry> operationEntries) {
        this.typeEntries = typeEntries;
        this.operationEntries = operationEntries;
        
        // Populate lookup
        this.nameToOperation = operationEntries.stream().collect(Collectors.toMap(operation -> operation.getName(),
                Function.identity()));
    }

    /**
     * Maps the parameter data for the given operation.
     * @param operationName The name of the desired operation
     * @param source The source data to map
     * @param target A buffer to receive the mapped data
     */
    public void mapParameterFor(String operationName, ByteBuffer source, ByteBuffer target) {
        this.mapDataForOperation(operationName, OperationEntry::getParameterMappingOperation, source, target);
    }
    
    /**
     * Maps the result data for the given operation.
     * @param operationName The name of the desired operation
     * @param source The source data to map
     * @param target A buffer to receive the mapped data
     */
    public void mapResultFor(String operationName, ByteBuffer source, ByteBuffer target) {
        this.mapDataForOperation(operationName, OperationEntry::getResultMappingOperation, source, target);
    }
    
    private void mapDataForOperation(String operationName, Function<OperationEntry, 
            ApiMappingOperation> operationProvider, ByteBuffer source, ByteBuffer target) {        
        OperationEntry operationEntry = this.nameToOperation.get(operationName);
        if (operationEntry == null) {
            throw new IllegalArgumentException("No entry for operation '" + operationName + "'.");
        }
        
        ApiMappingOperation operation = operationProvider.apply(operationEntry);
        operation.apply(0, this::resolveTypeEntry, source, target);
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
    
    /**
     * Returns the operation entries of this mapping script.
     * 
     * @return see above
     */
    public List<OperationEntry> getOperationEntries() {
        return Collections.unmodifiableList(this.operationEntries);
    }

}
