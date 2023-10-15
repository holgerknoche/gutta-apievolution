package gutta.apievolution.repository;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ProviderApiLoader;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for the consumer API service.
 */
class ConsumerApisServiceTest {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    /**
     * Test case: A consumer API is saved, and the referenced provider definition
     * exists and matches.
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
        verify(repositoryMock, times(1)).saveConsumerApiDefinition(any(PersistentConsumerApiDefinition.class));
    }

    /**
     * Test case: A consumer API shall be saved, but the referenced provider
     * definition does not exist.
     */
    @Test
    void saveWithMissingProviderRevision() {
        final String testHistoryName = "test";
        final int testRevision = 0;

        ProviderApisService providerServiceMock = mock(ProviderApisService.class);
        when(providerServiceMock.readApiRevision(any(String.class), any(int.class))).thenReturn(Optional.empty());

        ConsumerApisService service = new ConsumerApisService();
        service.providerApisService = providerServiceMock;

        ApiProcessingException exception = assertThrows(ApiProcessingException.class,
                () -> service.saveConsumerApi(testHistoryName, testRevision, "testConsumer",
                        "api test { record A { string fieldA as fieldX } }"));

        assertTrue(exception.getMessage().contains("revision does not exist"));
    }

    /**
     * Test case: A consumer API shall be saved, but the referenced provider
     * definition is incompatible.
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
        return this.createMapping("providerApi.api", "consumerApi.api", "json", mappingType);
    }
    
    private byte[] createScriptMapping(ApiMappingType mappingType) {
        return this.createMapping("boundedProviderApi.api", "boundedConsumerApi.api", "mappingscript", mappingType);
    }

    private byte[] createMapping(String providerDefinitionName, String consumerDefinitionName, String format, ApiMappingType mappingType) {
        String providerDefinitionText = this.readFileFromClasspath("apis/clientMapping/" + providerDefinitionName);
        String consumerDefinitionText = this.readFileFromClasspath("apis/clientMapping/" + consumerDefinitionName);

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
        when(providerServiceMock.readSupportedRevisions("testHistory")).thenReturn(Collections.singleton(0));

        ConsumerApisService service = new ConsumerApisService();
        service.apisRepository = repositoryMock;
        service.providerApisService = providerServiceMock;

        return service.mapConsumerApi(1, format, mappingType).getData();
    }
    
    private static Set<JsonNode> toJsonNodeSet(String json) {
        ArrayNode arrayNode;
        try {
            arrayNode = OBJECT_MAPPER.readValue(json, ArrayNode.class);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }

        Set<JsonNode> nodeSet = new HashSet<>(arrayNode.size());
        arrayNode.forEach(nodeSet::add);
        return nodeSet;
    }

    /**
     * Test case: Mapping of a consumer API against a provider revision in JSON for
     * the consumer side.
     */
    @Test
    void testJSONClientMappingForConsumer() {
        byte[] mappingBytes = this.createJsonMapping(ApiMappingType.CONSUMER);

        // The actual order is not deterministic, we therefore compare sets
        String expectedJson = "[{\"$type\":\"enum\",\"publicName\":\"E\",\"internalName\":\"EConsumer\",\"members\":[{\"publicName\":\"MEMBER_1\",\"internalName\":\"MEMBER_1_CONSUMER\"}]}, {\"$type\":\"record\",\"publicName\":\"A\",\"internalName\":\"AConsumer\",\"fields\":[{\"publicName\":\"fieldA\",\"internalName\":\"fieldAConsumer\"}]}, {\"$type\":\"operation\",\"publicName\":\"testOperation\",\"internalName\":\"testOperation\"}]";
        Set<JsonNode> expectedNodeSet = toJsonNodeSet(expectedJson);

        String actualJson = new String(mappingBytes, StandardCharsets.UTF_8);
        Set<JsonNode> actualNodeSet = toJsonNodeSet(actualJson);

        assertEquals(expectedNodeSet, actualNodeSet);
    }

    /**
     * Test case: Mapping of a consumer API against a provider revision in JSON for
     * the provider side.
     */
    @Test
    void testJSONClientMappingForProvider() {
        byte[] mappingBytes = this.createJsonMapping(ApiMappingType.PROVIDER);

        // The actual order is not deterministic, we therefore compare sets
        String expectedJson = "[{\"$type\":\"enum\",\"publicName\":\"E\",\"internalName\":\"EProvider\",\"members\":[{\"publicName\":\"MEMBER_1\",\"internalName\":\"MEMBER_1_PROVIDER\"}]},{\"$type\":\"record\",\"publicName\":\"A\",\"internalName\":\"AProvider\",\"fields\":[{\"publicName\":\"fieldA\",\"internalName\":\"fieldAProvider\"}]},{\"$type\":\"operation\",\"publicName\":\"testOperation\",\"internalName\":\"testOperation\"}]";
        Set<JsonNode> expectedNodeSet = toJsonNodeSet(expectedJson);

        String actualJson = new String(mappingBytes, StandardCharsets.UTF_8);
        Set<JsonNode> actualNodeSet = toJsonNodeSet(actualJson);

        assertEquals(expectedNodeSet, actualNodeSet);        
    }

    /**
     * Test case: Full mapping (consumer-internal to provider-internal) of a
     * consumer API against a provider revision in JSON.
     */
    @Test
    void testFullJSONClientMapping() {
        byte[] mappingBytes = this.createJsonMapping(ApiMappingType.FULL);

        // The actual order is not deterministic, we therefore compare sets
        String expectedJson = "[{\"$type\":\"record\",\"providerName\":\"AProvider\",\"consumerName\":\"AConsumer\",\"fields\":[{\"providerName\":\"fieldAProvider\",\"consumerName\":\"fieldAConsumer\"}]},{\"$type\":\"enum\",\"providerName\":\"EProvider\",\"consumerName\":\"EConsumer\",\"members\":[{\"providerName\":\"MEMBER_1_PROVIDER\",\"consumerName\":\"MEMBER_1_CONSUMER\"}]},{\"$type\":\"operation\",\"providerName\":\"testOperation\",\"consumerName\":\"testOperation\"}]";
        Set<JsonNode> expectedNodeSet = toJsonNodeSet(expectedJson);

        String actualJson = new String(mappingBytes, StandardCharsets.UTF_8);
        Set<JsonNode> actualNodeSet = toJsonNodeSet(actualJson);

        assertEquals(expectedNodeSet, actualNodeSet);
    }
    
    /**
     * Test case: Client mapping represented as a mapping script.
     */
    @Test
    void testMappingScriptForConsumer() {
        byte[] mappingBytes = this.createScriptMapping(ApiMappingType.CONSUMER);
        
        // We currently only assert that data is returned at all
        assertTrue(mappingBytes.length > 0);
    }

    /**
     * Test case: Provider mapping represented as a mapping script.
     */
    @Test
    void testMappingScriptForProvider() {
        byte[] mappingBytes = this.createScriptMapping(ApiMappingType.PROVIDER);
        
        // We currently only assert that data is returned at all
        assertTrue(mappingBytes.length > 0);
    }
    
}
