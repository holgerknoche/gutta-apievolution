package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for the API mapping script codec.
 */
class ApiMappingScriptCodecTest {
    
    /**
     * Test case: Serialization and deserialization of a record type with elementary mapping operations works as expected.
     * Note that the offsets and values are sort of arbitrary, as the script is not executed.
     */
    @Test
    void scriptForRecordWithElementaryOperations() {                
        // Copy mapping operation
        FieldMapping copyFieldMapping = new FieldMapping(0, new CopyOperation(1)); 
        
        // List mapping operation
        ApiMappingOperation elementOperation = new CopyOperation(2);
        FieldMapping listFieldMapping = new FieldMapping(1, new ListMappingOperation(5, 2, 2, elementOperation));
        
        // Enum mapping operation
        EnumTypeEntry embeddedEnumTypeEntry = new EnumTypeEntry(1, 1, new int[] {4, 3, 2, 1});
        FieldMapping enumFieldMapping = new FieldMapping(11, new EnumMappingOperation(embeddedEnumTypeEntry));
        
        // Skip operation
        FieldMapping skipFieldMapping = new FieldMapping(16, new SkipOperation(4));
        
        List<FieldMapping> fieldMappings = asList(
                copyFieldMapping,
                listFieldMapping,
                enumFieldMapping,
                skipFieldMapping
        );
        
        // Create surrounding record type entry and operation entry
        RecordTypeEntry recordTypeEntry = new RecordTypeEntry(0, 0, 20, fieldMappings);
        OperationEntry operationEntry = new OperationEntry(0, "op", new RecordMappingOperation(recordTypeEntry), new RecordMappingOperation(recordTypeEntry));
                
        ApiMappingScript script = new ApiMappingScript(asList(recordTypeEntry, embeddedEnumTypeEntry), singletonList(operationEntry));
        ApiMappingScriptCodec codec = new ApiMappingScriptCodec();
        
        byte[] expectedBytes = new byte[] {
                // --- Offsets
                0x00, 0x00, 0x00, 0x08, // Offset of the type list
                0x00, 0x00, 0x00, 0x6B, // Offset of the operation list
                // --- Type entries
                0x00, 0x00, 0x00, 0x02, // Number of type entries
                0x00, 0x00, 0x00, 0x14, // Offset of the first type entry 
                0x00, 0x00, 0x00, 0x52, // Offset of the second type entry                
                0x02, // Entry type for the first type entry (record type)
                0x00, 0x00, 0x00, 0x00, // Type id of the first type
                0x00, 0x00, 0x00, 0x14, // Data size of the first type
                0x00, 0x00, 0x00, 0x04, // Number of field mappings in the type
                0x00, 0x00, 0x00, 0x00, // Offset of the first field
                0x01, // Operation type (copy)
                0x00, 0x00, 0x00, 0x01, // Number of bytes to copy 
                0x00, 0x00, 0x00, 0x01, // Offset of the second field
                0x05, // Operation type (list mapping)
                0x00, 0x00, 0x00, 0x05, // Max size of the list
                0x00, 0x00, 0x00, 0x02, // Source element size
                0x00, 0x00, 0x00, 0x02, // Target element size
                0x01, // Element operation type (copy)
                0x00, 0x00, 0x00, 0x02, // Number of bytes to copy
                0x00, 0x00, 0x00, 0x0B, // Offset of the third field
                0x03, // Operation type (enum mapping)
                0x00, 0x00, 0x00, 0x01, // Entry index of the enum type
                0x00, 0x00, 0x00, 0x10, // Offset of the fourth field
                0x02, // Element operation type (skip)
                0x00, 0x00, 0x00, 0x04, // Number of bytes to skip
                0x01, // Entry type for the second type entry (enum type) 
                0x00, 0x00, 0x00, 0x01, // Type id of the second type 
                0x00, 0x00, 0x00, 0x04, // Number of member map entries 
                0x00, 0x00, 0x00, 0x04, // Mapping for value 0
                0x00, 0x00, 0x00, 0x03, // Mapping for value 1
                0x00, 0x00, 0x00, 0x02, // Mapping for value 2
                0x00, 0x00, 0x00, 0x01, // Mapping for value 3
                // --- Operation entries
                0x00, 0x00, 0x00, 0x01, // Number of operation entries 
                0x00, 0x00, 0x00, 0x73, // Offset of the first operation entry
                0x00, 0x00, 0x00, 0x02, // Length of operation name (in bytes) 
                0x6F, 0x70, // Name of the operation 
                0x04, // Operation type of the parameter mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x00, // Type index of the parameter type 
                0x04, // Operation type of the result mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x00 // Type index of the result type
                };
        
        byte[] scriptBytes = codec.encodeScript(script);
        assertArrayEquals(expectedBytes, scriptBytes);
        
        ApiMappingScript decodedScript = codec.decodeScript(scriptBytes);
        assertEquals(script, decodedScript);
    }
    
    /**
     * Test case: Serialization and deserialization of a script where a polymorphic type is used as a parameter and/or result type.
     */
    @Test
    void scriptWithPolymorphicParameterType() {
        // First subtype
        FieldMapping fieldAMapping = new FieldMapping(0, new CopyOperation(5));
        RecordTypeEntry typeAEntry = new RecordTypeEntry(0, 0, 5, singletonList(fieldAMapping));
        
        // Second subtype
        FieldMapping fieldBMapping = new FieldMapping(0, new CopyOperation(10));
        RecordTypeEntry typeBEntry = new RecordTypeEntry(1, 1, 10, singletonList(fieldBMapping));
        
        PolymorphicRecordMapping typeAMapping = new PolymorphicRecordMapping(5, 0, typeAEntry);
        PolymorphicRecordMapping typeBMapping = new PolymorphicRecordMapping(6, 1, typeBEntry); 
        PolymorphicRecordMappingOperation polyMappingOperation = new PolymorphicRecordMappingOperation(Arrays.asList(typeAMapping, typeBMapping));
        
        OperationEntry operationEntry = new OperationEntry(0, "op", polyMappingOperation, polyMappingOperation);
        
        ApiMappingScript script = new ApiMappingScript(asList(typeAEntry, typeBEntry), singletonList(operationEntry));
        ApiMappingScriptCodec codec = new ApiMappingScriptCodec();
        
        byte[] expectedBytes = new byte[] {
                // --- Offsets
                0x00, 0x00, 0x00, 0x08, // Offset of the type list
                0x00, 0x00, 0x00, 0x40, // Offset of the operation list                
                // --- Type entries
                0x00, 0x00, 0x00, 0x02, // Number of type entries
                0x00, 0x00, 0x00, 0x14, // Offset of the first type entry
                0x00, 0x00, 0x00, 0x2A, // Offset of the second type entry
                0x02, // Entry type for the first type (record type)
                0x00, 0x00, 0x00, 0x00, // Type id of the first type
                0x00, 0x00, 0x00, 0x05, // Data size of the first type
                0x00, 0x00, 0x00, 0x01, // Number of field mappings
                0x00, 0x00, 0x00, 0x00, // Offset of the first field
                0x01, // Operation type of the mapping (copy)
                0x00, 0x00, 0x00, 0x05, // Number of bytes to copy
                0x02, // Entry type for the second type (record type)
                0x00, 0x00, 0x00, 0x01, // Type id of the second type
                0x00, 0x00, 0x00, 0x0A, // Data size of the second type
                0x00, 0x00, 0x00, 0x01, // Number of field mappings
                0x00, 0x00, 0x00, 0x00, // Offset of the first field
                0x01, // Operation type of the mapping (copy)
                0x00, 0x00, 0x00, 0x0A, // Number of bytes to copy
                // --- Operation entries
                0x00, 0x00, 0x00, 0x01, // Number of operation entries
                0x00, 0x00, 0x00, 0x48, // Offset of the first operation entry
                0x00, 0x00, 0x00, 0x02, // Length of the operation name (in bytes)
                0x6F, 0x70, // Name of the operation
                0x06, // Operation type of the parameter mapping operation (poly record mapping)
                0x00, 0x00, 0x00, 0x02, // Number of type mappings
                0x00, 0x00, 0x00, 0x05, // Source type id of the first mapping
                0x00, 0x00, 0x00, 0x00, // Target type id of the first mapping
                0x00, 0x00, 0x00, 0x00, // Type index of the first mapping
                0x00, 0x00, 0x00, 0x06, // Source type id of the second mapping
                0x00, 0x00, 0x00, 0x01, // Target type id of the second mapping
                0x00, 0x00, 0x00, 0x01, // Type index of the second mapping
                0x06, // Operation type of the result mapping operation (poly record mapping)
                0x00, 0x00, 0x00, 0x02, // Number of type mappings
                0x00, 0x00, 0x00, 0x05, // Source type id of the first mapping
                0x00, 0x00, 0x00, 0x00, // Target type id of the first mapping
                0x00, 0x00, 0x00, 0x00, // Type index of the first mapping
                0x00, 0x00, 0x00, 0x06, // Source type id of the second mapping
                0x00, 0x00, 0x00, 0x01, // Target type id of the second mapping
                0x00, 0x00, 0x00, 0x01 // Type index of the second mapping
        };
        
        byte[] scriptBytes = codec.encodeScript(script);
        assertArrayEquals(expectedBytes, scriptBytes);
        
        ApiMappingScript decodedScript = codec.decodeScript(scriptBytes);
        assertEquals(script, decodedScript);
    }
    
    /**
     * Test case: Serialization and deserialization of a script where a polymorphic type is used within a record type.
     */
    @Test
    void scriptWithEmbeddedPolymorphicType() {
        // First subtype
        FieldMapping fieldAMapping = new FieldMapping(0, new CopyOperation(5));
        RecordTypeEntry typeAEntry = new RecordTypeEntry(0, 0, 5, singletonList(fieldAMapping));
        
        // Second subtype
        FieldMapping fieldBMapping = new FieldMapping(0, new CopyOperation(10));
        RecordTypeEntry typeBEntry = new RecordTypeEntry(1, 1, 10, singletonList(fieldBMapping));
        
        PolymorphicRecordMapping typeAMapping = new PolymorphicRecordMapping(5, 0, typeAEntry);
        PolymorphicRecordMapping typeBMapping = new PolymorphicRecordMapping(6, 1, typeBEntry); 
        PolymorphicRecordMappingOperation polyMappingOperation = new PolymorphicRecordMappingOperation(Arrays.asList(typeAMapping, typeBMapping));
        
        // Type containing the polymorphic mapping
        FieldMapping polyFieldMapping = new FieldMapping(0, polyMappingOperation);
        RecordTypeEntry containerTypeEntry = new RecordTypeEntry(2, 2, 10, singletonList(polyFieldMapping));
        
        OperationEntry operationEntry = new OperationEntry(0, "op", new RecordMappingOperation(containerTypeEntry), new RecordMappingOperation(containerTypeEntry));
        
        ApiMappingScript script = new ApiMappingScript(asList(typeAEntry, typeBEntry, containerTypeEntry), singletonList(operationEntry));
        ApiMappingScriptCodec codec = new ApiMappingScriptCodec();
        
        byte[] expectedBytes = new byte[] {
                // --- Offsets
                0x00, 0x00, 0x00, 0x08, // Offset of the type list
                0x00, 0x00, 0x00, 0x72, // Offset of the operation list
                // --- Type entries
                0x00, 0x00, 0x00, 0x03, // Number of type entries
                0x00, 0x00, 0x00, 0x18, // Offset of the first type entry
                0x00, 0x00, 0x00, 0x2E, // Offset of the second type entry
                0x00, 0x00, 0x00, 0x44, // Offset of the third type entry                
                0x02, // Entry type for the first type (record type)
                0x00, 0x00, 0x00, 0x00, // Type id of the first type
                0x00, 0x00, 0x00, 0x05, // Data size of the first type
                0x00, 0x00, 0x00, 0x01, // Number of field mappings
                0x00, 0x00, 0x00, 0x00, // Offset of the first field
                0x01, // Operation type of the mapping (copy)
                0x00, 0x00, 0x00, 0x05, // Number of bytes to copy
                0x02, // Entry type for the second type (record type)
                0x00, 0x00, 0x00, 0x01, // Type id of the second type
                0x00, 0x00, 0x00, 0x0A, // Data size of the second type
                0x00, 0x00, 0x00, 0x01, // Number of field mappings
                0x00, 0x00, 0x00, 0x00, // Offset of the first field
                0x01, // Operation type of the mapping (copy)
                0x00, 0x00, 0x00, 0x0A, // Number of bytes to copy
                0x02, // Entry type for the third type (record type)
                0x00, 0x00, 0x00, 0x02, // Type id of the third type
                0x00, 0x00, 0x00, 0x0A, // Data size of the third type
                0x00, 0x00, 0x00, 0x01, // Number of field mappings
                0x00, 0x00, 0x00, 0x00, // Offset of the first field
                0x06, // Operation type of the mapping (poly record mapping)
                0x00, 0x00, 0x00, 0x02, // Number of type mappings
                0x00, 0x00, 0x00, 0x05, // Source type id of the first mapping
                0x00, 0x00, 0x00, 0x00, // Target type id of the first mapping
                0x00, 0x00, 0x00, 0x00, // Type index of the first mapping
                0x00, 0x00, 0x00, 0x06, // Source type id of the second mapping
                0x00, 0x00, 0x00, 0x01, // Target type id of the second mapping
                0x00, 0x00, 0x00, 0x01, // Type index of the second mapping
                // --- Operation entries
                0x00, 0x00, 0x00, 0x01, // Number of operation entries
                0x00, 0x00, 0x00, 0x7A, // Offset of the first operation entry
                0x00, 0x00, 0x00, 0x02, // Length of the operation name (in bytes)
                0x6F, 0x70, // Name of the operation
                0x04, // Operation type of the parameter mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x02, // Type index of the record
                0x04, // Operation type of the result mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x02 // Type index of the record
        };
        
        byte[] scriptBytes = codec.encodeScript(script);
        assertArrayEquals(expectedBytes, scriptBytes);
        
        ApiMappingScript decodedScript = codec.decodeScript(scriptBytes);
        assertEquals(script, decodedScript);
    }
    
    /**
     * Test case: Serialization and deserialization of a script with multiple operations.
     */
    @Test
    void scriptWithMultipleOperations() {
        RecordTypeEntry typeEntry = new RecordTypeEntry(0, 0, 0, emptyList());
        
        OperationEntry operationEntry1 = new OperationEntry(0, "op1", new RecordMappingOperation(typeEntry), new RecordMappingOperation(typeEntry));
        OperationEntry operationEntry2 = new OperationEntry(1, "op2", new RecordMappingOperation(typeEntry), new RecordMappingOperation(typeEntry));        
        
        ApiMappingScript script = new ApiMappingScript(singletonList(typeEntry), asList(operationEntry1, operationEntry2));
        ApiMappingScriptCodec codec = new ApiMappingScriptCodec();
        
        byte[] expectedBytes = new byte[] {
             // --- Offsets
                0x00, 0x00, 0x00, 0x08, // Offset of the type list
                0x00, 0x00, 0x00, 0x1D, // Offset of the operation list
                // --- Type entries
                0x00, 0x00, 0x00, 0x01, // Number of type entries
                0x00, 0x00, 0x00, 0x10, // Offset of the first type entry                
                0x02, // Entry type for the first type (record type)
                0x00, 0x00, 0x00, 0x00, // Type id of the first type
                0x00, 0x00, 0x00, 0x00, // Data length of the first type
                0x00, 0x00, 0x00, 0x00, // Number of field mappings
                // --- Operation entries
                0x00, 0x00, 0x00, 0x02, // Number of operation entries
                0x00, 0x00, 0x00, 0x29, // Offset of the first operation entry
                0x00, 0x00, 0x00, 0x3A, // Offset of the second operation entry
                0x00, 0x00, 0x00, 0x03, // Length of the operation name (in bytes)
                0x6F, 0x70, 0x31, // Name of the operation
                0x04, // Operation type of the parameter mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x00, // Type index of the record
                0x04, // Operation type of the result mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x00, // Type index of the record
                0x00, 0x00, 0x00, 0x03, // Length of the operation name (in bytes)
                0x6F, 0x70, 0x32, // Name of the operation
                0x04, // Operation type of the parameter mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x00, // Type index of the record
                0x04, // Operation type of the result mapping operation (record mapping)
                0x00, 0x00, 0x00, 0x00 // Type index of the record
        };
        
        byte[] scriptBytes = codec.encodeScript(script);
        assertArrayEquals(expectedBytes, scriptBytes);
        
        ApiMappingScript decodedScript = codec.decodeScript(scriptBytes);
        assertEquals(script, decodedScript);
    }

}
