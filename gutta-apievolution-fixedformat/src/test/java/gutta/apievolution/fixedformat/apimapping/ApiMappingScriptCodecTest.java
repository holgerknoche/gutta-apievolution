package gutta.apievolution.fixedformat.apimapping;

import org.junit.jupiter.api.Test;

import java.util.List;

import static java.util.Arrays.asList;
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
                // --- Type entries
                0x00, 0x00, 0x00, 0x02, // Number of type entries
                0x00, 0x00, 0x00, 0x10, // Offset of the first type entry 
                0x00, 0x00, 0x00, 0x4E, // Offset of the second type entry
                0x00, 0x00, 0x00, 0x67, // Offset of the operation list
                0x02, // Type flag for the first type entry (record type)
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
                0x01, // Type flag for the second type entry (enum type) 
                0x00, 0x00, 0x00, 0x01, // Type id of the second type 
                0x00, 0x00, 0x00, 0x04, // Number of member map entries 
                0x00, 0x00, 0x00, 0x04, // Mapping for value 0
                0x00, 0x00, 0x00, 0x03, // Mapping for value 1
                0x00, 0x00, 0x00, 0x02, // Mapping for value 2
                0x00, 0x00, 0x00, 0x01, // Mapping for value 3
                // --- Operation entries
                0x00, 0x00, 0x00, 0x01, // Number of operation entries 
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

}
