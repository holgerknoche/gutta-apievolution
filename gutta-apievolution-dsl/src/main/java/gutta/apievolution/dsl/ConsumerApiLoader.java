package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.io.IOException;
import java.io.InputStream;

/**
 * Utility class for loading consumer API definitions.
 */
public class ConsumerApiLoader extends ApiDefinitionLoader {

    /**
     * Loads an API definition from the given string.
     *
     * @param input              The string to read the definition from
     * @param referencedApiName  The name of the referenced provider API
     * @param referencedRevision The revision number referenced in the provider
     *                           history
     * @return The loaded API definition
     */
    public static ConsumerApiDefinition loadFromString(String input, String referencedApiName, int referencedRevision) {
        ApiRevisionParser.ApiDefinitionContext specification = parseString(input);
        return buildDefinition("<none>", specification, referencedApiName, referencedRevision);
    }

    /**
     * Loads an API definition from the given input stream.
     *
     * @param inputStream        The input stream to read the definition from
     * @param sourceName         The name of the source (e.g., the file name)
     * @param referencedApiName  The name of the referenced provider API
     * @param referencedRevision The revision number referenced in the provider
     *                           history
     * @return The loaded API definition
     */
    public static ConsumerApiDefinition loadFromStream(InputStream inputStream, String sourceName, String referencedApiName, int referencedRevision) {
        try {
            ApiRevisionParser.ApiDefinitionContext specification = parseStream(inputStream);
            return buildDefinition(sourceName, specification, referencedApiName, referencedRevision);
        } catch (IOException e) {
            throw new ApiLoadException("Error loading API definition.", e);
        }
    }

    private static ConsumerApiDefinition buildDefinition(String sourceName, ApiRevisionParser.ApiDefinitionContext specification, String referencedApiName,
            int referencedRevision) {
        ConsumerApiRevisionModelBuilderPass1 pass1 = new ConsumerApiRevisionModelBuilderPass1(sourceName);
        ConsumerApiRevisionModelBuilderPass2 pass2 = new ConsumerApiRevisionModelBuilderPass2(sourceName);

        ConsumerApiDefinition apiDefinition = pass1.buildConsumerRevision(specification, referencedApiName, referencedRevision);
        pass2.augmentConsumerRevision(specification, apiDefinition);

        apiDefinition.finalizeDefinition();

        return apiDefinition;
    }

    /**
     * Loads a consumer API definition from the given file on the classpath.
     *
     * @param fileName           The file name of the file to load the definition
     *                           from
     * @param referencedApiName  The name of the referenced provider API
     * @param referencedRevision The provider revision referenced by this definition
     * @return The loaded API definition
     */
    public static ConsumerApiDefinition loadFromClasspath(String fileName, String referencedApiName, int referencedRevision) {
        ClassLoader classLoader = ConsumerApiLoader.class.getClassLoader();

        try (InputStream inputStream = classLoader.getResourceAsStream(fileName)) {
            return loadFromStream(inputStream, fileName, referencedApiName, referencedRevision);
        } catch (IOException e) {
            throw new ApiLoadException("Error loading API definition " + fileName + " from classpath.", e);
        }
    }

}
