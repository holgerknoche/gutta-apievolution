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
import gutta.apievolution.fixedformat.consumer.ConsumerSubTypeB;
import gutta.apievolution.fixedformat.consumer.ConsumerSuperType;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.provider.ProviderParameter;
import gutta.apievolution.fixedformat.provider.ProviderResult;
import gutta.apievolution.fixedformat.provider.ProviderSuperType;
import org.junit.jupiter.api.Test;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class FixedFormatMappingTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", "test.provider", 0);
    
    private static final RevisionHistory REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api"); 
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(REVISION_HISTORY, new HashSet<>(Arrays.asList(0, 1)), CONSUMER_API);
    
    /**
     * Test case: The invocation of a method with conversion of simple fields works as expected.
     */
    @Test
    void testFixedFormatConversation() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);

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
    
    /**
     * Test case: The invocation of a method with polymorphic parameter and result works as expected.
     */
    @Test
    void immediatePolymorphicTypeConversion() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);

        FixedFormatMapper mapper = new FixedFormatMapper();
        
        PolyOperationProviderProxy providerProxy = new PolyOperationProviderProxy(consumerToProviderScript, providerToConsumerScript, mapper);
        RequestRouter requestRouter = new RequestRouter(providerProxy);
        
        PolyOperationConsumerProxy consumerProxy = new PolyOperationConsumerProxy(requestRouter, mapper);
        
        ConsumerSubTypeB parameter = new ConsumerSubTypeB();
        parameter.setFieldB(1234);
        
        ConsumerSuperType result = consumerProxy.invoke(parameter);
        
        assertNotSame(result, parameter);
        assertEquals(result, parameter);
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
    
    private static class PolyOperationConsumerProxy extends ConsumerOperationProxy<ConsumerSuperType, ConsumerSuperType> {

        public PolyOperationConsumerProxy(RequestRouter router, FixedFormatMapper mapper) {
            super("polyOperation", ConsumerSuperType.class, ConsumerSuperType.class, router, mapper, CHARSET);
        }
        
    }
    
    private static class PolyOperationProviderProxy extends ProviderOperationProxy<ProviderSuperType, ProviderSuperType> {
        
        public PolyOperationProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper) {
            super("polyOperation", ProviderSuperType.class, ProviderSuperType.class, consumerToProviderScript, providerToConsumerScript, mapper, CHARSET);
        }
        
        @Override
        protected ProviderSuperType invokeOperation(ProviderSuperType parameter) {
            return parameter;
        }
        
    }

}
