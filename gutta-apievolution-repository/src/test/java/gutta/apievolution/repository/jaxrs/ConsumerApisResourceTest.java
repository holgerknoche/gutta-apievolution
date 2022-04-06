package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ConsumerApisService;
import gutta.apievolution.repository.PersistentConsumerApiDefinition;
import gutta.apievolution.repository.PersistentProviderApiDefinition;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Optional;
import javax.ws.rs.core.Response;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for the consumer API resource.
 */
class ConsumerApisResourceTest {

    /**
     * Test case: Successful read of an existing definition.
     */
    @Test
    void successfulRead() {
        final LocalDateTime testCommitTime = LocalDateTime.of(2022, 1, 1, 0, 0, 0, 0);

        PersistentProviderApiDefinition referencedDefinition = new PersistentProviderApiDefinition();
        referencedDefinition.setHistoryName("testHistory");
        referencedDefinition.setRevisionNumber(1);

        PersistentConsumerApiDefinition testDefinition = new PersistentConsumerApiDefinition();
        testDefinition.setId(1);
        testDefinition.setReferencedRevision(referencedDefinition);
        testDefinition.setCommitTime(testCommitTime);
        testDefinition.setConsumerName("testConsumer");
        testDefinition.setDefinitionText("api test {}");

        ConsumerApisService serviceMock = mock(ConsumerApisService.class);
        when(serviceMock.readConsumerApi(1)).thenReturn(Optional.of(testDefinition));

        ConsumerApisResource resource = new ConsumerApisResource();
        resource.objectMapper = new ObjectMapperProvider().getObjectMapper();
        resource.apisService = serviceMock;

        Response response = resource.readConsumerApi(1);
        String expectedJson = "{\"id\":1,\"commitTime\":[2022,1,1,0,0],\"consumerName\":\"testConsumer\"," +
                "\"referencedHistoryName\":\"testHistory\",\"referencedRevisionNumber\":1," +
                "\"definition\":\"api test {}\"}";

        assertEquals(200, response.getStatus());
        assertEquals(expectedJson, new String((byte[]) response.getEntity(), StandardCharsets.UTF_8));
    }

    /**
     * Test case: Read of a non-existing definition.
     */
    @Test
    void readOfNonExistingDefinition() {
        ConsumerApisService serviceMock = mock(ConsumerApisService.class);
        when(serviceMock.readConsumerApi(any(Integer.class))).thenReturn(Optional.empty());

        ConsumerApisResource resource = new ConsumerApisResource();
        resource.objectMapper = new ObjectMapperProvider().getObjectMapper();
        resource.apisService = serviceMock;

        Response response = resource.readConsumerApi(1);

        assertEquals(404, response.getStatus());
    }

    @Test
    void saveDefinitionSuccessfully() {
        String requestJson = "{\"referencedHistoryName\": \"test\"," + "\"referencedRevisionNumber\": 1," +
                "\"consumerName\": \"testConsumer\"," + "\"definition\": \"api test {}\"}";

        final LocalDateTime testCommitTime = LocalDateTime.of(2022, 1, 1, 0, 0, 0, 0);

        PersistentProviderApiDefinition referencedDefinition = new PersistentProviderApiDefinition();
        referencedDefinition.setHistoryName("testHistory");
        referencedDefinition.setRevisionNumber(1);

        PersistentConsumerApiDefinition testDefinition = new PersistentConsumerApiDefinition();
        testDefinition.setId(1);
        testDefinition.setCommitTime(testCommitTime);
        testDefinition.setReferencedRevision(referencedDefinition);
        testDefinition.setConsumerName("testConsumer");
        testDefinition.setDefinitionText("api test {}");

        ConsumerApisService serviceMock = mock(ConsumerApisService.class);
        when(serviceMock.saveConsumerApi(any(String.class), any(int.class), any(String.class), any(String.class)))
                .thenReturn(testDefinition);

        ConsumerApisResource resource = new ConsumerApisResource();
        resource.objectMapper = new ObjectMapperProvider().getObjectMapper();
        resource.apisService = serviceMock;

        Response response = resource.saveConsumerApi(requestJson.getBytes(StandardCharsets.UTF_8));

        String expectedJson = "{\"id\":1,\"commitTime\":[2022,1,1,0,0]}";

        assertEquals(200, response.getStatus());
        assertEquals(expectedJson, new String((byte[]) response.getEntity(), StandardCharsets.UTF_8));
    }

}
