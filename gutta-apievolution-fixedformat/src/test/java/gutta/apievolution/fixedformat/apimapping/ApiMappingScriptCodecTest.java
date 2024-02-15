package gutta.apievolution.fixedformat.apimapping;

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ApiMappingScriptCodecTest {
    
    @Test
    void scriptForNonPolymorphicRecordType() {
        FieldMapping intFieldMapping = new FieldMapping(0, new CopyOperation(5)); 
        
        List<FieldMapping> fieldMappings = asList(
                intFieldMapping
        );
        
        RecordTypeEntry typeEntry = new RecordTypeEntry(0, 0, 14, fieldMappings);
        //OperationEntry operationEntry = new OperationEntry(0, "op", new RecordMappingOperation(typeEntry), new RecordMappingOperation(typeEntry));
                
        ApiMappingScript script = new ApiMappingScript(singletonList(typeEntry), Collections.emptyList());
        ApiMappingScriptCodec codec = new ApiMappingScriptCodec();
        
        byte[] scriptBytes = codec.encodeScript(script);
        // TODO assertArrayEquals()
        
        ApiMappingScript decodedScript = codec.decodeScript(scriptBytes);
        assertEquals(script, decodedScript);
    }

}
