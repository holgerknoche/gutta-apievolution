package gutta.apievolution.fixedformat;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.NamedInputStream;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptCodec;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.Objects.requireNonNull;

/**
 * Tool to generate and export a mapping script to a file.
 */
public class MappingScriptExporter {

    /**
     * Entry point of the exporter tool.
     * 
     * @param arguments The command line arguments to use
     * @throws IOException If an I/O error occurs
     */
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
        Set<Integer> supportedRevisions = new HashSet<>();
        MappingDirection mappingDirection = null;

        int currentArgumentIndex = 0;
        while (currentArgumentIndex < arguments.length) {
            String currentArgument = arguments[currentArgumentIndex++];

            switch (currentArgument) {
            case "-c":
            case "--consumer-api":
                consumerApiName = arguments[currentArgumentIndex++];
                break;

            case "-n":
            case "--referenced-api":
                referencedApiName = arguments[currentArgumentIndex++];
                break;

            case "-r":
            case "--referenced-revision":
                referencedRevision = Integer.parseInt(arguments[currentArgumentIndex++]);
                break;

            case "-o":
            case "--output":
                outputScriptName = arguments[currentArgumentIndex++];
                break;

            case "-s":
            case "--supported-revision":
                supportedRevisions.add(Integer.valueOf(arguments[currentArgumentIndex++]));
                break;

            case "-p":
            case "--provider-revision":
                providerRevisionNames.add(arguments[currentArgumentIndex++]);
                break;

            case "-t":
            case "--type":
                String type = arguments[currentArgumentIndex++];
                switch (type) {
                case "consumer":
                    mappingDirection = MappingDirection.CONSUMER_TO_PROVIDER;
                    break;

                case "provider":
                    mappingDirection = MappingDirection.PROVIDER_TO_CONSUMER;
                    break;

                default:
                    throw new IllegalArgumentException("Unknown type '" + type + "',  must be 'consumer' or 'provider'.");
                }
                break;

            default:
                break;
            }
        }

        requireNonNull(consumerApiName, "No consumer API given.");
        requireNonNull(referencedApiName, "No referenced API name given.");
        requireNonNull(mappingDirection, "No type given.");
        requireNonNull(outputScriptName, "No output script name given.");

        return new Arguments(consumerApiName, referencedApiName, referencedRevision, providerRevisionNames, supportedRevisions, mappingDirection,
                outputScriptName);
    }

    void exportScript(Arguments arguments) {
        // Load the consumer API
        ConsumerApiDefinition consumerApi = this.loadConsumerApi(arguments);

        // Load the provider revision history
        RevisionHistory revisionHistory = this.loadRevisionHistory(arguments);

        // Resolve the APIs against each other and generate the mapping script
        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, arguments.supportedRevisions,
                consumerApi);
        ApiMappingScript mappingScript = new ApiMappingScriptGenerator().generateMappingScript(definitionResolution, arguments.mappingDirection);

        // Write the mapping script
        this.writeScript(arguments.outputScriptName, mappingScript);
    }

    private ConsumerApiDefinition loadConsumerApi(Arguments arguments) {
        String consumerApiName = arguments.consumerApiName;

        try (InputStream inputStream = this.openInputStream(consumerApiName)) {
            return ConsumerApiLoader.loadFromStream(inputStream, consumerApiName, arguments.referencedApiName, arguments.referencedRevision);
        } catch (IOException e) {
            throw new ExportException("Error loading consumer API '" + consumerApiName + "'.", e);
        }
    }

    InputStream openInputStream(String streamName) throws IOException {
        return new FileInputStream(streamName);
    }

    private RevisionHistory loadRevisionHistory(Arguments arguments) {
        List<NamedInputStream> inputStreams = arguments.providerRevisionNames.stream().map(streamName -> new NamedInputStream(streamName, () -> {
            try {
                return this.openInputStream(streamName);
            } catch (IOException e) {
                throw new ExportException("Error loading provider API '" + streamName + "'.", e);
            }
        })).collect(Collectors.toList());

        List<ProviderApiDefinition> providerApis = ProviderApiLoader.loadHistoryFromStreams(IntegerRange.unbounded(), false, inputStreams);
        return new RevisionHistory(providerApis);
    }

    private void writeScript(String fileName, ApiMappingScript script) {
        byte[] scriptBytes = new ApiMappingScriptCodec().encodeScript(script);

        try (OutputStream outputStream = this.openOutputStream(fileName)) {
            outputStream.write(scriptBytes);
        } catch (IOException e) {
            throw new ExportException("Error writing script to '" + fileName + "'.", e);
        }
    }

    OutputStream openOutputStream(String outputName) throws IOException {
        return new FileOutputStream(outputName);
    }

    static class Arguments {

        public final String consumerApiName;

        public final String referencedApiName;

        public final int referencedRevision;

        public final List<String> providerRevisionNames;

        public final Set<Integer> supportedRevisions;

        public final MappingDirection mappingDirection;

        public final String outputScriptName;

        public Arguments(String consumerApiName, String referencedApiName, int referencedRevision, List<String> providerRevisionNames,
                Set<Integer> supportedRevisions, MappingDirection mappingDirection, String outputScriptName) {
            this.consumerApiName = consumerApiName;
            this.referencedApiName = referencedApiName;
            this.referencedRevision = referencedRevision;
            this.providerRevisionNames = providerRevisionNames;
            this.supportedRevisions = supportedRevisions;
            this.mappingDirection = mappingDirection;
            this.outputScriptName = outputScriptName;
        }

    }

    static class ExportException extends RuntimeException {

        private static final long serialVersionUID = 527635247776530245L;

        public ExportException(String message, Throwable cause) {
            super(message, cause);
        }

    }

}
