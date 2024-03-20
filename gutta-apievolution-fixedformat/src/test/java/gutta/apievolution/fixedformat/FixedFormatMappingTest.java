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
import gutta.apievolution.fixedformat.consumer.ConsumerStructureWithPolyField;
import gutta.apievolution.fixedformat.consumer.ConsumerSubTypeA;
import gutta.apievolution.fixedformat.consumer.ConsumerSubTypeB;
import gutta.apievolution.fixedformat.consumer.ConsumerSuperType;
import gutta.apievolution.fixedformat.consumer.ConsumerTestException;
import gutta.apievolution.fixedformat.consumer.MappedConsumerTestException;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.objectmapping.UnrepresentableValueException;
import gutta.apievolution.fixedformat.provider.MappableProviderTestException;
import gutta.apievolution.fixedformat.provider.ProviderParameter;
import gutta.apievolution.fixedformat.provider.ProviderResult;
import gutta.apievolution.fixedformat.provider.ProviderStructureWithPolyField;
import gutta.apievolution.fixedformat.provider.ProviderSuperType;
import gutta.apievolution.fixedformat.provider.ProviderTestException;
import org.junit.jupiter.api.Test;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
    
    /**
     * Test case: The invocation of a method with parameter and result structures that contain polymorphic values works as expected.
     */
    @Test
    void indirectPolymorphicTypeConversion() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);

        FixedFormatMapper mapper = new FixedFormatMapper();
        
        PolyOperation2ProviderProxy providerProxy = new PolyOperation2ProviderProxy(consumerToProviderScript, providerToConsumerScript, mapper);
        RequestRouter requestRouter = new RequestRouter(providerProxy);
        
        PolyOperation2ConsumerProxy consumerProxy = new PolyOperation2ConsumerProxy(requestRouter, mapper);
                
        ConsumerSubTypeA subType = new ConsumerSubTypeA();
        subType.setFieldA("Test");
        
        ConsumerStructureWithPolyField parameter = new ConsumerStructureWithPolyField();
        parameter.setField(subType);
        
        ConsumerStructureWithPolyField result = consumerProxy.invoke(parameter);
        
        assertNotSame(result, parameter);
        assertEquals(result, parameter);
    }
    
    /**
     * Test case: A thrown exception is mapped as expected.
     */
    @Test
    void exceptionMapping() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);

        FixedFormatMapper mapper = new FixedFormatMapper();

        OpWithExceptionProviderProxy providerProxy = new OpWithExceptionProviderProxy(consumerToProviderScript, providerToConsumerScript, mapper);
        RequestRouter requestRouter = new RequestRouter(providerProxy);
        
        OpWithExceptionConsumerProxy consumerProxy = new OpWithExceptionConsumerProxy(requestRouter, mapper);
        
        MappedConsumerTestException thrownException = assertThrows(MappedConsumerTestException.class, () -> consumerProxy.invoke(new ConsumerParameter()));
        assertEquals(1234, thrownException.getExceptionField());
    }
    
    // TODO Test case for monomorphic to polymorphic conversion and vice versa
    
    /**
     * Test case: The provider throws an exception, but the consumer does not expect one. This results in an unrepresentable value.
     */
    @Test
    void providerThrowsExceptionButConsumerDoesNotExpectOne() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.CONSUMER_TO_PROVIDER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);

        FixedFormatMapper mapper = new FixedFormatMapper();
        
        OpWithUnmappedExceptionProviderProxy providerProxy = new OpWithUnmappedExceptionProviderProxy(consumerToProviderScript, providerToConsumerScript, mapper);
        RequestRouter requestRouter = new RequestRouter(providerProxy);
        
        OpWithUnmappedExceptionConsumerProxy consumerProxy = new OpWithUnmappedExceptionConsumerProxy(requestRouter, mapper);
        assertThrows(UnrepresentableValueException.class, () -> consumerProxy.invoke(new ConsumerParameter()));
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
    
    private static class PolyOperation2ConsumerProxy extends ConsumerOperationProxy<ConsumerStructureWithPolyField, ConsumerStructureWithPolyField> {

        public PolyOperation2ConsumerProxy(RequestRouter router, FixedFormatMapper mapper) {
            super("polyOperation2", ConsumerStructureWithPolyField.class, ConsumerStructureWithPolyField.class, router, mapper, CHARSET);
        }
        
    }
    
    private static class PolyOperation2ProviderProxy extends ProviderOperationProxy<ProviderStructureWithPolyField, ProviderStructureWithPolyField> {
        
        public PolyOperation2ProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper) {
            super("polyOperation2", ProviderStructureWithPolyField.class, ProviderStructureWithPolyField.class, consumerToProviderScript, providerToConsumerScript, mapper, CHARSET);
        }
        
        @Override
        protected ProviderStructureWithPolyField invokeOperation(ProviderStructureWithPolyField parameter) {
            return parameter;
        }
        
    }
    
    private static class OpWithExceptionConsumerProxy extends ConsumerOperationProxy<ConsumerParameter, ConsumerResult> {
        
        public OpWithExceptionConsumerProxy(RequestRouter router, FixedFormatMapper mapper) {
            super("opWithException", ConsumerParameter.class, ConsumerResult.class, Collections.singleton(ConsumerTestException.class), router, mapper, CHARSET);
        }
                
    }
    
    private static class OpWithExceptionProviderProxy extends ProviderOperationProxy<ProviderParameter, ProviderResult> {

        public OpWithExceptionProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper) {
            super("opWithException", ProviderParameter.class, ProviderResult.class, Collections.singleton(ProviderTestException.class), consumerToProviderScript, providerToConsumerScript, mapper, CHARSET);
        }

        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            ProviderTestException exceptionData = new ProviderTestException();
            exceptionData.setExceptionField(1234);
            
            throw new MappableProviderTestException(exceptionData);
        }
        
    }
    
    private static class OpWithUnmappedExceptionConsumerProxy extends ConsumerOperationProxy<ConsumerParameter, ConsumerResult> {
        
        public OpWithUnmappedExceptionConsumerProxy(RequestRouter router, FixedFormatMapper mapper) {
            super("opWithUnmappedException", ConsumerParameter.class, ConsumerResult.class, Collections.emptySet(), router, mapper, CHARSET);
        }
                
    }
    
    private static class OpWithUnmappedExceptionProviderProxy extends ProviderOperationProxy<ProviderParameter, ProviderResult> {

        public OpWithUnmappedExceptionProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper) {
            super("opWithUnmappedException", ProviderParameter.class, ProviderResult.class, Collections.singleton(ProviderTestException.class), consumerToProviderScript, providerToConsumerScript, mapper, CHARSET);
        }

        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            ProviderTestException exceptionData = new ProviderTestException();
            exceptionData.setExceptionField(1234);
            
            throw new MappableProviderTestException(exceptionData);
        }
        
    }

}
