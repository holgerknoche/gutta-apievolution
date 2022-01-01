package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.dsl.parser.ApiRevisionLexer;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.*;

import java.io.IOException;
import java.io.InputStream;

/**
 * Utility class for loading consumer API definitions.
 */
public class ConsumerApiLoader extends ApiDefinitionLoader {

    /**
     * Loads an API definition from the given input stream.
     * @param inputStream The input stream to read the definition from
     * @param referencedRevision The revision number referenced in the provider history
     * @return The loaded API definition
     * @throws IOException If an I/O error occurs while loading the definition
     */
    public static ConsumerApiDefinition loadFromStream(InputStream inputStream, int referencedRevision)
            throws IOException {
        ApiRevisionParser.ApiDefinitionContext specification = parseStream(inputStream);
        return new ConsumerApiRevisionModelBuilder().buildConsumerRevision(referencedRevision, specification);
    }

}
