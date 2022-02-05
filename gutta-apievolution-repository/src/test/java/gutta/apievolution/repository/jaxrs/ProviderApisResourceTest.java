package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.PersistentProviderApiDefinition;
import gutta.apievolution.repository.ProviderApisService;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Optional;
import javax.ws.rs.core.Response;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests for the provider APIs resource.
 */
class ProviderApisResourceTest {

    /**
     * Test case: Successful read of an existing definition.
     */
    @Test
    void successfulRead() {
        final String testHistory = "test";
        final int testRevisionNumber = 0;
        final LocalDateTime testCommitTime = LocalDateTime.of(2022, 1, 1, 0, 0, 0, 0);
        final String testDefinitionText = "api test {}";

        PersistentProviderApiDefinition testDefinition = new PersistentProviderApiDefinition();
        testDefinition.setHistoryName(testHistory);
        testDefinition.setRevisionNumber(testRevisionNumber);
        testDefinition.setCommitTime(testCommitTime);
        testDefinition.setSupportedFrom(testCommitTime);
        testDefinition.setSupportedUntil(LocalDateTime.MAX);
        testDefinition.setDefinitionText(testDefinitionText);

        ProviderApisService serviceMock = mock(ProviderApisService.class);
        when(serviceMock.readApiRevision(testHistory, testRevisionNumber))
                .thenReturn(Optional.of(testDefinition));

        ProviderApisResource resource = new ProviderApisResource();
        resource.objectMapper = new ObjectMapperProvider().getObjectMapper();
        resource.apisService = serviceMock;

        Response response = resource.readProviderApiRevision(testHistory, testRevisionNumber);

        String expectedJson = "{\"historyName\":\"test\",\"revisionNumber\":0,\"commitTime\":[2022,1,1,0,0]," +
                "\"supportedFrom\":[2022,1,1,0,0],\"supportedUntil\":[999999999,12,31,23,59,59,999999999]," +
                "\"definition\":\"api test {}\"}";

        assertEquals(200, response.getStatus());
        assertEquals(expectedJson, new String((byte[]) response.getEntity(), StandardCharsets.UTF_8));
    }

    /**
     * Test case: Read of a non-existing definition.
     */
    @Test
    void readOfNonExistingRevision() {
        ProviderApisService serviceMock = mock(ProviderApisService.class);

        when(serviceMock.readApiRevision(any(String.class), any(int.class))).thenReturn(Optional.empty());

        ProviderApisResource resource = new ProviderApisResource();
        resource.objectMapper = new ObjectMapperProvider().getObjectMapper();
        resource.apisService = serviceMock;

        Response response = resource.readProviderApiRevision("doesNotExist", 0);

        assertEquals(404, response.getStatus());
        assertNull(response.getEntity());
    }

}
