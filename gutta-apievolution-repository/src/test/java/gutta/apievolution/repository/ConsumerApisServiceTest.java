package gutta.apievolution.repository;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.temporal.TemporalUnit;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
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

}
