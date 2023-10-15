package gutta.apievolution.inprocess;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.inprocess.consumer.ConsumerApi;
import gutta.apievolution.inprocess.consumer.ConsumerEnum;
import gutta.apievolution.inprocess.consumer.ConsumerParameter;
import gutta.apievolution.inprocess.consumer.ConsumerResult;

class InProcessMappingTest {

    @Test
    void successfulInvocation() {
        // Load the consumer and provider API definitions
        ConsumerApiDefinition consumerApiDefinition = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);

        RevisionHistory providerRevisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api",
                "apis/provider-revision-2.api");
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));

        // Resolve consumer definition against the provider history
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(providerRevisionHistory, supportedRevisions,
                consumerApiDefinition);

        // TODO Pass the resolution to the API resolver
        ConsumerApi consumerApi = ApiResolver.resolveApi(consumerApiDefinition, ConsumerApi.class);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestEnum(ConsumerEnum.VALUE_A);
        parameter.setTestField("someValue");
        parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        ConsumerResult result = consumerApi.testOperation(parameter);

        // assertEquals(ConsumerEnum.VALUE_A, result.getResultEnum());
        // assertEquals("someValue", result.getRetField());
        // assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A),
        // result.getResultList());
    }

}
