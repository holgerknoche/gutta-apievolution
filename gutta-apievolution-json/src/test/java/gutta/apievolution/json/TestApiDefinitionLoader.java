package gutta.apievolution.json;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class TestApiDefinitionLoader {

    public static RevisionHistory loadRevisionHistory(String... fileNames) {
        ClassLoader classLoader = TestApiDefinitionLoader.class.getClassLoader();
        List<InputStream> streams = Stream.of(fileNames).map(classLoader::getResourceAsStream).filter(Objects::nonNull)
                .collect(Collectors.toList());

        try {
            List<ProviderApiDefinition> apiDefinitions = ProviderApiLoader
                    .loadHistoryFromStreams(IntegerRange.unbounded(), false, streams);
            RevisionHistory revisionHistory = new RevisionHistory(apiDefinitions);

            for (InputStream stream : streams) {
                stream.close();
            }

            return revisionHistory;
        } catch (IOException e) {
            throw new ApiLoadFailedException(e);
        }
    }

    public static ConsumerApiDefinition loadConsumerApi(String fileName, int referencedRevision) {
        try (InputStream inputStream = TestApiDefinitionLoader.class.getClassLoader().getResourceAsStream(fileName)) {
            return ConsumerApiLoader.loadFromStream(inputStream, referencedRevision);
        } catch (IOException e) {
            throw new ApiLoadFailedException(e);
        }
    }

    private static class ApiLoadFailedException
            extends RuntimeException {

        public ApiLoadFailedException(Throwable cause) {
            super(cause);
        }

    }

}
