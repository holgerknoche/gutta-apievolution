package gutta.apievolution.fixedformat.apimapping;

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ApiMappingScriptCodecTest {
    
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
        
        RecordTypeEntry recordTypeEntry = new RecordTypeEntry(0, 0, 14, fieldMappings);
        OperationEntry operationEntry = new OperationEntry(0, "op", new RecordMappingOperation(recordTypeEntry), new RecordMappingOperation(recordTypeEntry));
                
        ApiMappingScript script = new ApiMappingScript(asList(recordTypeEntry, embeddedEnumTypeEntry), Collections.emptyList());
        ApiMappingScriptCodec codec = new ApiMappingScriptCodec();
        
        byte[] scriptBytes = codec.encodeScript(script);
        // TODO assertArrayEquals()
        
        ApiMappingScript decodedScript = codec.decodeScript(scriptBytes);
        assertEquals(script, decodedScript);
    }

}
