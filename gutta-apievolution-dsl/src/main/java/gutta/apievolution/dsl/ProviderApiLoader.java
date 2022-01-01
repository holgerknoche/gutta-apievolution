package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.dsl.parser.ApiRevisionLexer;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * Utility class for loading provider API definitions.
 */
public class ProviderApiLoader extends ApiDefinitionLoader {

    /**
     * Loads an API definition from the given input stream.
     * @param revision The revision number to assign to the definition
     * @param inputStream The input stream to read the definition from
     * @return The loaded definition
     * @throws IOException If an I/O error occurs while loading the definition
     */
    public static ProviderApiDefinition loadFromStream(final int revision, final InputStream inputStream)
            throws IOException {
        return loadFromStream(revision, inputStream, Optional.empty());
    }

    /**
     * Loads an API definition from the given input stream and resolves it against an optional predecessor.
     * @param revision The revision number to assign to the definition
     * @param inputStream The input stream to read the definition from
     * @param optionalPredecessor An optional predecessor to resolve the loaded definition against
     * @return The loaded and resolved definition
     * @throws IOException If an I/O error occurs while loading the definition
     */
    public static ProviderApiDefinition loadFromStream(final int revision, final InputStream inputStream,
                                                       final Optional<ProviderApiDefinition> optionalPredecessor)
            throws IOException {
        ApiRevisionParser.ApiDefinitionContext specification = parseStream(inputStream);
        return new ProviderApiRevisionModelBuilder().buildProviderRevision(revision, specification,
                optionalPredecessor);
    }

    /**
     * Loads a revision history from a collection of streams, drawing revision numbers from the given iterable.
     * @param revisionIds An iterable providing a sufficient number of revision numbers
     * @param streams The streams to load the definitions from
     * @return The loaded and resolved revision history
     * @throws IOException If an I/O error occurs while loading the definitions
     */
    public static List<ProviderApiDefinition> loadHistoryFromStreams(final Iterable<Integer> revisionIds,
                                                                     final Collection<? extends InputStream> streams)
            throws IOException {
        if (streams.isEmpty()) {
            return Collections.emptyList();
        }

        int streamCount = streams.size();
        List<ProviderApiDefinition> revisions = new ArrayList<ProviderApiDefinition>(streamCount);
        Iterator<Integer> revisionsIdsIterator = revisionIds.iterator();
        Iterator<? extends InputStream> streamIterator = streams.iterator();

        // Convert the first stream (i.e. first revision in the history)
        ProviderApiDefinition currentRevision;
        try (InputStream stream = streamIterator.next()) {
            currentRevision = loadFromStream(revisionsIdsIterator.next(), stream);
            revisions.add(currentRevision);
        }

        // Convert the remaining streams
        while (streamIterator.hasNext()) {
            try (InputStream stream = streamIterator.next()) {
                Optional<ProviderApiDefinition> optionalPredecessor = Optional.of(currentRevision);
                currentRevision = loadFromStream(revisionsIdsIterator.next(), stream, optionalPredecessor);
                revisions.add(currentRevision);
            }
        }

        return revisions;
    }

}
