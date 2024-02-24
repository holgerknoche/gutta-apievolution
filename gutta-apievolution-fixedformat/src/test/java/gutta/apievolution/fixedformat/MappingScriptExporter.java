package gutta.apievolution.fixedformat;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.dsl.ConsumerApiLoader;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static java.util.Objects.*;

public class MappingScriptExporter {
    
    public static void main(String[] arguments) throws IOException {
        Arguments parsedArguments = parseArguments(arguments);
        
        new MappingScriptExporter().exportScript(parsedArguments);
    }
    
    private static Arguments parseArguments(String[] arguments) {
        String consumerApiName = null;
        List<String> providerRevisionNames = new ArrayList<>();
        String referencedApiName = null;
        int referencedRevision = 0;
        String outputScriptName = null;
    
        int currentArgumentIndex = 0;
        while (currentArgumentIndex < arguments.length) {
            String currentArgument = arguments[currentArgumentIndex++];
            
            switch (currentArgument) {
            case "-c":
                consumerApiName = arguments[currentArgumentIndex++];
                break;
                
            case "-n":
                referencedApiName = arguments[currentArgumentIndex++];
                break;
                
            case "-r":
                referencedRevision = Integer.parseInt(arguments[currentArgumentIndex++]);
                break;
                
            case "-o":
                outputScriptName = arguments[currentArgumentIndex++];
                break;
                
            case "-p":
                providerRevisionNames.add(arguments[currentArgumentIndex++]);
                break;
                
            default:
                break;
            }
        }
        
        requireNonNull(consumerApiName, "No consumer API given.");
        requireNonNull(referencedApiName, "No referenced API name given.");
        requireNonNull(outputScriptName, "No output script name given.");
                
        return new Arguments(consumerApiName, referencedApiName, referencedRevision, providerRevisionNames, outputScriptName);
    }
    
    private void exportScript(Arguments arguments) {
        String consumerApiName = arguments.consumerApiName;
        
        ConsumerApiDefinition consumerApi;
        try (FileInputStream inputStream = new FileInputStream(consumerApiName)) {
            consumerApi = ConsumerApiLoader.loadFromStream(null, null, null, 0);
        } catch (IOException e) {
            throw new ExportException("Error opening consumer API '" + consumerApiName + "'.", e); 
        }
        
        // TODO
    }
    
    private static class Arguments {
        
        public final String consumerApiName;
        
        public final String referencedApiName;
        
        public final int referencedRevision;
        
        public final List<String> providerRevisionNames;
        
        public final String outputScriptName;
        
        public Arguments(String consumerApiName, String referencedApiName, int referencedRevision, List<String> providerRevisionNames, String outputScriptName) {
            this.consumerApiName = consumerApiName;
            this.referencedApiName = referencedApiName;
            this.referencedRevision = referencedRevision;
            this.providerRevisionNames = providerRevisionNames;
            this.outputScriptName = outputScriptName;
        }
        
    }
    
    private static class ExportException extends RuntimeException {
        
        public ExportException(String message, Throwable cause) {
            super(message, cause);
        }
        
    }

}
