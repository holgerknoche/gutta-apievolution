package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * This class represents an API mapping script for converting a fixed-format representation of an API into another.
 */
public class ApiMappingScript {    
    
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
        operation.apply(0, source, target);
    }
    
    /**
     * Returns the type entries of this mapping script.
     * 
     * @return see above
     */
    public List<TypeEntry> getTypeEntries() {
        return Collections.unmodifiableList(this.typeEntries);
    }
    
    /**
     * Returns the operation entries of this mapping script.
     * 
     * @return see above
     */
    public List<OperationEntry> getOperationEntries() {
        return Collections.unmodifiableList(this.operationEntries);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(this.typeEntries, this.operationEntries);
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(ApiMappingScript that) {
        return this.typeEntries.equals(that.typeEntries) &&
               this.operationEntries.equals(that.operationEntries);
    }

}
