package gutta.apievolution.json;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.NamedInputStream;
import gutta.apievolution.dsl.ProviderApiLoader;

class TestApiDefinitionLoader {

    public static RevisionHistory loadRevisionHistory(String... fileNames) {
        try {
            List<NamedInputStream> streams = loadStreams(fileNames);

            List<ProviderApiDefinition> apiDefinitions = ProviderApiLoader.loadHistoryFromStreams(IntegerRange.unbounded(), false, streams);
            RevisionHistory revisionHistory = new RevisionHistory(apiDefinitions);

            for (NamedInputStream stream : streams) {
                stream.close();
            }

            return revisionHistory;
        } catch (IOException e) {
            throw new ApiLoadFailedException(e);
        }
    }

    private static List<NamedInputStream> loadStreams(String... fileNames) throws IOException {
        ClassLoader classLoader = TestApiDefinitionLoader.class.getClassLoader();

        List<NamedInputStream> streams = new ArrayList<>(fileNames.length);
        for (String fileName : fileNames) {
            InputStream inputStream = classLoader.getResourceAsStream(fileName);

            if (inputStream != null) {
                streams.add(new NamedInputStream(fileName, inputStream));
            }
        }

        return streams;

    }

    public static ConsumerApiDefinition loadConsumerApi(String fileName, String referencedApiName, int referencedRevision) {
        try (InputStream inputStream = TestApiDefinitionLoader.class.getClassLoader().getResourceAsStream(fileName)) {
            return ConsumerApiLoader.loadFromStream(inputStream, fileName, referencedApiName, referencedRevision);
        } catch (IOException e) {
            throw new ApiLoadFailedException(e);
        }
    }

    private static class ApiLoadFailedException extends RuntimeException {

        private static final long serialVersionUID = -3031184149542951350L;

        public ApiLoadFailedException(Throwable cause) {
            super(cause);
        }

    }

}
