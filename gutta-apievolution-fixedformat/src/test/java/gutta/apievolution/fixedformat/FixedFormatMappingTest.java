package gutta.apievolution.fixedformat;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;
import gutta.apievolution.fixedformat.consumer.ConsumerEnum;
import gutta.apievolution.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.fixedformat.consumer.ConsumerResult;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.provider.ProviderParameter;
import gutta.apievolution.fixedformat.provider.ProviderResult;
import org.junit.jupiter.api.Test;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FixedFormatMappingTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    @Test
    void testFixedFormatConversation() {
        // Generate the required API mapping scripts
        ConsumerApiDefinition consumerApi = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api");

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, new HashSet<>(Arrays.asList(0, 1)), consumerApi);
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(resolution, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(resolution, MappingDirection.PROVIDER_TO_CONSUMER);

        FixedFormatMapper mapper = new FixedFormatMapper();
        
        TestOperationProviderProxy providerProxy = new TestOperationProviderProxy(consumerToProviderScript, providerToConsumerScript, mapper);
        RequestRouter requestRouter = new RequestRouter(providerProxy);
        
        TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter, mapper);
        
        ConsumerParameter parameter = new ConsumerParameter().testField("TestString").testEnum(ConsumerEnum.VALUE_A)
                .testList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        ConsumerResult consumerResult = consumerProxy.invoke(parameter);        

        assertEquals("ret: TestString", consumerResult.getResultField());
        assertEquals(ConsumerEnum.VALUE_A, consumerResult.getResultEnum());
        assertEquals(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B), consumerResult.getResultList());
    }

    private static class TestOperationConsumerProxy extends ConsumerOperationProxy<ConsumerParameter, ConsumerResult> {

        public TestOperationConsumerProxy(RequestRouter router, FixedFormatMapper mapper) {
            super("testOperation", ConsumerParameter.class, ConsumerResult.class, router, mapper, CHARSET);
        }

    }
    
    private static class TestOperationProviderProxy extends ProviderOperationProxy<ProviderParameter, ProviderResult> {

        public TestOperationProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper) {
            super("testOperation", ProviderParameter.class, ProviderResult.class, consumerToProviderScript, providerToConsumerScript, mapper, CHARSET);
        }

        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            ProviderResult result = new ProviderResult();

            result.setResultEnum(parameter.getTestEnum());
            result.setRetField("ret: " + parameter.getFieldA());
            result.setResultList(parameter.getTestList());

            return result;
        }
        
    }

}
