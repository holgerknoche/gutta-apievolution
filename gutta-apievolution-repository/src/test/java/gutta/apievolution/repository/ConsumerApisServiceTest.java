package gutta.apievolution.repository;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ProviderApiLoader;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * Test cases for the consumer API service.
 */
class ConsumerApisServiceTest {

    /**
     * Test case: A consumer API is saved, and the referenced provider definition exists and matches.
     */
    @Test
    void saveWithMatchingProviderRevision() {
        final String testHistoryName = "test";
        final int testRevision = 0;

        PersistentProviderApiDefinition providerDefinition = new PersistentProviderApiDefinition();
        providerDefinition.setHistoryName(testHistoryName);
        providerDefinition.setRevisionNumber(testRevision);
        providerDefinition.setSupportedFrom(LocalDateTime.now());
        providerDefinition.setSupportedUntil(LocalDateTime.MAX);
        providerDefinition.setDefinitionText("api test { record A { string fieldA } }");

        ProviderApisService providerServiceMock = mock(ProviderApisService.class);
        when(providerServiceMock.readApiRevision(testHistoryName, testRevision))
                .thenReturn(Optional.of(providerDefinition));

        ConsumerApisRepository repositoryMock = mock(ConsumerApisRepository.class);

        ConsumerApisService service = new ConsumerApisService();
        service.providerApisService = providerServiceMock;
        service.apisRepository = repositoryMock;

        service.saveConsumerApi(testHistoryName, testRevision, "testConsumer",
                "api test { record A { string fieldA as fieldX } }");

        // Assert that the persist method is actually invoked
        verify(repositoryMock, times(1))
                .saveConsumerApiDefinition(any(PersistentConsumerApiDefinition.class));
    }

    /**
     * Test case: A consumer API shall be saved, but the referenced provider definition does not exist.
     */
    @Test
    void saveWithMissingProviderRevision() {
        final String testHistoryName = "test";
        final int testRevision = 0;

        ProviderApisService providerServiceMock = mock(ProviderApisService.class);
        when(providerServiceMock.readApiRevision(any(String.class), any(int.class)))
                .thenReturn(Optional.empty());

        ConsumerApisService service = new ConsumerApisService();
        service.providerApisService = providerServiceMock;

        ApiProcessingException exception = assertThrows(ApiProcessingException.class,
                () -> service.saveConsumerApi(testHistoryName, testRevision, "testConsumer",
                "api test { record A { string fieldA as fieldX } }"));

        assertTrue(exception.getMessage().contains("revision does not exist"));
    }

    /**
     * Test case: A consumer API shall be saved, but the referenced provider definition is incompatible.
     */
    @Test
    void saveWithIncompatibleProviderRevision() {
        final String testHistoryName = "test";
        final int testRevision = 0;

        PersistentProviderApiDefinition providerDefinition = new PersistentProviderApiDefinition();
        providerDefinition.setHistoryName(testHistoryName);
        providerDefinition.setRevisionNumber(testRevision);
        providerDefinition.setSupportedFrom(LocalDateTime.now());
        providerDefinition.setSupportedUntil(LocalDateTime.MAX);
        providerDefinition.setDefinitionText("api test { record A { string fieldA } }");

        ProviderApisService providerServiceMock = mock(ProviderApisService.class);
        when(providerServiceMock.readApiRevision(testHistoryName, testRevision))
                .thenReturn(Optional.of(providerDefinition));

        ConsumerApisService service = new ConsumerApisService();
        service.providerApisService = providerServiceMock;

        ApiProcessingException exception = assertThrows(ApiProcessingException.class,
                () -> service.saveConsumerApi(testHistoryName, testRevision, "testConsumer",
                "api test { record B { string fieldB } }"));

        assertTrue(exception.getMessage().contains("definition is incompatible"));
    }

    @Test
    void testWithUnsupportedProviderRevision() {
        final String testHistoryName = "test";
        final int testRevision = 0;

        LocalDateTime currentTime = LocalDateTime.now();

        // Create a definition that has already expired
        PersistentProviderApiDefinition providerDefinition = new PersistentProviderApiDefinition();
        providerDefinition.setHistoryName(testHistoryName);
        providerDefinition.setRevisionNumber(testRevision);
        providerDefinition.setSupportedFrom(currentTime.minusDays(2));
        providerDefinition.setSupportedUntil(currentTime.minusDays(1));
        providerDefinition.setDefinitionText("api test { record A { string fieldA } }");

        ProviderApisService providerServiceMock = mock(ProviderApisService.class);
        when(providerServiceMock.readApiRevision(testHistoryName, testRevision))
                .thenReturn(Optional.of(providerDefinition));

        ConsumerApisRepository repositoryMock = mock(ConsumerApisRepository.class);

        ConsumerApisService service = new ConsumerApisService();
        service.providerApisService = providerServiceMock;
        service.apisRepository = repositoryMock;

        ApiProcessingException exception = assertThrows(ApiProcessingException.class,
                () -> service.saveConsumerApi(testHistoryName, testRevision, "testConsumer",
                "api test { record A { string fieldA as fieldX } }"));

        assertTrue(exception.getMessage().contains("is not supported"));
    }

    private String readFileFromClasspath(String fileName) {
        try (InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream(fileName)) {
            return (inputStream == null) ? "" : new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private byte[] createJsonMapping(ApiMappingType mappingType) {
        String providerDefinitionText = this.readFileFromClasspath("apis/clientMapping/providerApi.api");
        String consumerDefinitionText = this.readFileFromClasspath("apis/clientMapping/consumerApi.api");

        PersistentProviderApiDefinition persistentProviderDefinition = new PersistentProviderApiDefinition();
        persistentProviderDefinition.setHistoryName("testHistory");
        persistentProviderDefinition.setRevisionNumber(0);
        persistentProviderDefinition.setDefinitionText(providerDefinitionText);

        ProviderApiDefinition providerDefinition = ProviderApiLoader.loadFromString(0,
                persistentProviderDefinition.getDefinitionText(), false, Optional.empty());

        PersistentConsumerApiDefinition consumerDefinition = new PersistentConsumerApiDefinition();
        consumerDefinition.setReferencedRevision(persistentProviderDefinition);
        consumerDefinition.setDefinitionText(consumerDefinitionText);

        ConsumerApisRepository repositoryMock = mock(ConsumerApisRepository.class);
        when(repositoryMock.findById(1)).thenReturn(Optional.of(consumerDefinition));

        ProviderApisService providerServiceMock = mock(ProviderApisService.class);
        when(providerServiceMock.readRevisionHistory("testHistory"))
                .thenReturn(new RevisionHistory(providerDefinition));
        when(providerServiceMock.readSupportedRevisions("testHistory"))
                .thenReturn(Collections.singleton(0));

        ConsumerApisService service = new ConsumerApisService();
        service.apisRepository = repositoryMock;
        service.providerApisService = providerServiceMock;

        return service.mapConsumerApi(1, "json", mappingType);
    }

    /**
     * Test case: Mapping of a consumer API against a provider revision in JSON for the consumer side.
     */
    @Test
    void testJSONClientMappingForConsumer() {
        byte[] mappingBytes = this.createJsonMapping(ApiMappingType.CONSUMER);

        String expected = "[{\"$type\":\"record\",\"publicName\":\"A\",\"internalName\":\"AConsumer\",\"fields\":[{\"publicName\":\"fieldA\",\"internalName\":\"fieldAConsumer\"}]},{\"$type\":\"enum\",\"publicName\":\"E\",\"internalName\":\"EConsumer\",\"members\":[{\"publicName\":\"MEMBER_1\",\"internalName\":\"MEMBER_1_CONSUMER\"}]}]";
        String actual = new String(mappingBytes, StandardCharsets.UTF_8);

        assertEquals(expected,actual);
    }

    /**
     * Test case: Mapping of a consumer API against a provider revision in JSON for the provider side.
     */
    @Test
    void testJSONClientMappingForProvider() {
        byte[] mappingBytes = this.createJsonMapping(ApiMappingType.PROVIDER);

        String expected = "[{\"$type\":\"record\",\"publicName\":\"A\",\"internalName\":\"AProvider\",\"fields\":[{\"publicName\":\"fieldA\",\"internalName\":\"fieldAProvider\"}]},{\"$type\":\"enum\",\"publicName\":\"E\",\"internalName\":\"EProvider\",\"members\":[{\"publicName\":\"MEMBER_1\",\"internalName\":\"MEMBER_1_PROVIDER\"}]}]";
        String actual = new String(mappingBytes, StandardCharsets.UTF_8);

        assertEquals(expected,actual);
    }

    /**
     * Test case: Full mapping (consumer-internal to provider-internal) of a consumer API against a provider revision
     * in JSON.
     */
    @Test
    void testFullJSONClientMapping() {
        byte[] mappingBytes = this.createJsonMapping(ApiMappingType.FULL);

        String expected = "[{\"$type\":\"record\",\"providerName\":\"AProvider\",\"consumerName\":\"AConsumer\",\"fields\":[{\"providerName\":\"fieldAProvider\",\"consumerName\":\"fieldAConsumer\"}]},{\"$type\":\"enum\",\"providerName\":\"EProvider\",\"consumerName\":\"EConsumer\",\"members\":[{\"providerName\":\"MEMBER_1_PROVIDER\",\"consumerName\":\"MEMBER_1_CONSUMER\"}]}]";
        String actual = new String(mappingBytes, StandardCharsets.UTF_8);

        assertEquals(expected,actual);
    }

}
