package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Utility class for loading provider API definitions.
 */
public class ProviderApiLoader extends ApiDefinitionLoader {

    /**
     * Loads an API definition from the given input stream.
     * @param revision The revision number to assign to the definition
     * @param inputStream The input stream to read the definition from
     * @param ignoreReplacements Flag whether to ignore replacement clauses in the definition
     * @return The loaded definition
     * @throws IOException If an I/O error occurs while loading the definition
     */
    public static ProviderApiDefinition loadFromStream(final int revision, final InputStream inputStream,
                                                       boolean ignoreReplacements)
            throws IOException {
        return loadFromStream(revision, inputStream, ignoreReplacements, Optional.empty());
    }

    /**
     * Loads an API definition from the given input stream and resolves it against an optional predecessor.
     * @param revision The revision number to assign to the definition
     * @param inputStream The input stream to read the definition from
     * @param ignoreReplacements Flag whether to ignore replacement clauses in the definition
     * @param optionalPredecessor An optional predecessor to resolve the loaded definition against
     * @return The loaded and resolved definition
     * @throws IOException If an I/O error occurs while loading the definition
     */
    public static ProviderApiDefinition loadFromStream(final int revision, final InputStream inputStream,
                                                       boolean ignoreReplacements,
                                                       final Optional<ProviderApiDefinition> optionalPredecessor)
            throws IOException {
        ApiRevisionParser.ApiDefinitionContext specification = parseStream(inputStream);

        ProviderApiRevisionModelBuilderPass1 pass1 = new ProviderApiRevisionModelBuilderPass1();
        ProviderApiRevisionModelBuilderPass2 pass2 = new ProviderApiRevisionModelBuilderPass2();

        ProviderApiDefinition apiDefinition = pass1.buildProviderRevision(revision, specification, ignoreReplacements,
                optionalPredecessor);
        pass2.augmentProviderRevision(specification, apiDefinition, ignoreReplacements, optionalPredecessor);

        apiDefinition.finalizeDefinition();

        return apiDefinition;
    }

    /**
     * Loads a revision history from a collection of streams, drawing revision numbers from the given iterable.
     * @param revisionIds An iterable providing a sufficient number of revision numbers
     * @param ignoreReplacements Flag whether to ignore replacement clauses in the definition
     * @param streams The streams to load the definitions from
     * @return The loaded and resolved revision history
     */
    public static List<ProviderApiDefinition> loadHistoryFromStreams(final Iterable<Integer> revisionIds,
                                                                     boolean ignoreReplacements,
                                                                     final Collection<? extends InputStream> streams) {
        if (streams.isEmpty()) {
            return Collections.emptyList();
        }

        int streamCount = streams.size();
        List<ProviderApiDefinition> revisions = new ArrayList<>(streamCount);
        Iterator<Integer> revisionsIdsIterator = revisionIds.iterator();
        Iterator<? extends InputStream> streamIterator = streams.iterator();

        try {
            // Convert the first stream (i.e. first revision in the history)
            ProviderApiDefinition currentRevision;
            try (InputStream stream = streamIterator.next()) {
                currentRevision = loadFromStream(revisionsIdsIterator.next(), stream, ignoreReplacements);
                revisions.add(currentRevision);
            }

            // Convert the remaining streams
            while (streamIterator.hasNext()) {
                try (InputStream stream = streamIterator.next()) {
                    Optional<ProviderApiDefinition> optionalPredecessor = Optional.of(currentRevision);
                    currentRevision = loadFromStream(revisionsIdsIterator.next(), stream, ignoreReplacements,
                            optionalPredecessor);
                    revisions.add(currentRevision);
                }
            }
        } catch (IOException e) {
            throw new ApiLoadException("Error loading provider APIs.", e);
        }

        return revisions;
    }

    /**
     * Loads a revision history from the given files on the classpath.
     * @param fileNames The filenames on the classpath to load the revisions from
     * @return The loaded revision history
     */
    public static RevisionHistory loadHistoryFromClasspath(String... fileNames) {
        return loadHistoryFromClasspath(false, fileNames);
    }

    /**
     * Loads a revision history from the given files on the classpath.
     * @param ignoreReplacements Flag whether to ignore replacement clauses in the definition
     * @param fileNames The filenames on the classpath to load the revisions from
     * @return The loaded revision history
     */
    public static RevisionHistory loadHistoryFromClasspath(boolean ignoreReplacements, String... fileNames) {
        ClassLoader classLoader = ProviderApiLoader.class.getClassLoader();

        List<InputStream> inputStreams = Stream.of(fileNames)
                .map(classLoader::getResourceAsStream)
                .collect(Collectors.toList());

        return new RevisionHistory(loadHistoryFromStreams(IntegerRange.unbounded(), ignoreReplacements, inputStreams));
    }

}
