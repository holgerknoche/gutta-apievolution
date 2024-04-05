package gutta.apievolution.tools;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.tools.MappingScriptExporter.Arguments;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for the {@link MappingScriptExporter} tool.
 */
class MappingScriptExporterTest {
    
    /**
     * Test case: Exporting a script works without an error.
     */
    @Test
    void mappingScriptGeneration() {
        String consumerApiName = "apis/consumer-api.api";
        List<String> providerRevisionNames = Arrays.asList("apis/provider-revision-1.api", "apis/provider-revision-2.api");
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));
        
        Arguments arguments = new Arguments(consumerApiName, "test.provider", 0, providerRevisionNames, supportedRevisions, MappingDirection.CONSUMER_TO_PROVIDER, "output.dat");
        
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();        
        MappingScriptExporter exporter = new TestScriptExporter(outputStream);
        exporter.exportScript(arguments);
        
        byte[] writtenBytes = outputStream.toByteArray();
        assertTrue(writtenBytes.length > 0);
    }
    
    /**
     * Specific implementation of a script exporter that does not read from or write to files.
     */
    private static class TestScriptExporter extends MappingScriptExporter {
        
        private final OutputStream outputStream;
        
        public TestScriptExporter(OutputStream outputStream) {
            this.outputStream = outputStream;
        }
        
        @Override
        InputStream openInputStream(String streamName) throws IOException {
            ClassLoader classLoader = this.getClass().getClassLoader();
            InputStream stream = classLoader.getResourceAsStream(streamName); 
            
            if (stream != null) {
                return stream;
            } else {
                throw new FileNotFoundException("Cannot find resource '" + streamName + "' on the classpath.");
            }
        }
        
        @Override
        OutputStream openOutputStream(String outputName) throws IOException {
            return this.outputStream;
        }
        
    }

}
