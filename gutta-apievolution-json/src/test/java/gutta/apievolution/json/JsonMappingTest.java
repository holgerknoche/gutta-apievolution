package gutta.apievolution.json;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.consumer.ConsumerEnum;
import gutta.apievolution.json.consumer.ConsumerMonoToPolyType;
import gutta.apievolution.json.consumer.ConsumerOperationProxy;
import gutta.apievolution.json.consumer.ConsumerParameter;
import gutta.apievolution.json.consumer.ConsumerResult;
import gutta.apievolution.json.consumer.ConsumerStructureWithMonoToPolyField;
import gutta.apievolution.json.consumer.ConsumerStructureWithPolyField;
import gutta.apievolution.json.consumer.ConsumerSubTypeA;
import gutta.apievolution.json.consumer.ConsumerSubTypeB;
import gutta.apievolution.json.consumer.ConsumerSuperType;
import gutta.apievolution.json.consumer.ConsumerTestException;
import gutta.apievolution.json.consumer.ConsumerTestExceptionData;
import gutta.apievolution.json.consumer.UnrepresentableValueException;
import gutta.apievolution.json.provider.MappableProviderTestException;
import gutta.apievolution.json.provider.ProviderEnum;
import gutta.apievolution.json.provider.ProviderMonoToPolySubTypeA;
import gutta.apievolution.json.provider.ProviderMonoToPolyType;
import gutta.apievolution.json.provider.ProviderOperationProxy;
import gutta.apievolution.json.provider.ProviderParameter;
import gutta.apievolution.json.provider.ProviderResult;
import gutta.apievolution.json.provider.ProviderStructureWithMonoToPolyField;
import gutta.apievolution.json.provider.ProviderStructureWithPolyField;
import gutta.apievolution.json.provider.ProviderSuperType;
import gutta.apievolution.json.provider.ProviderTestException;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for the JSON mapping functionality.
 */
class JsonMappingTest {

    /**
     * Tests a simple, JSON-based conversation between a consumer and a provider.
     */
    @Test
    void testJsonConversation() {
        TestOperationProviderProxy providerProxy = new TestOperationProviderProxy();
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestField("test value");
        parameter.setTestEnum(ConsumerEnum.VALUE_A);
        parameter.setTestList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));

        TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter);
        ConsumerResult result = consumerProxy.invokeOperation(parameter);

        assertEquals("test valueX", result.getResultField());
        assertEquals(ConsumerEnum.VALUE_B, result.getResultEnum());
        assertEquals(Arrays.asList(ConsumerEnum.VALUE_B, ConsumerEnum.VALUE_A), result.getResultList());
    }
    
    /**
     * Test case: The invocation of a method with polymorphic parameter and result works as expected.
     */
    @Test
    void immediatePolymorphicTypes() {
        PolyOperationProviderProxy providerProxy = new PolyOperationProviderProxy();
        RequestRouter router = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerSubTypeB parameter = new ConsumerSubTypeB();
        parameter.setFieldB(1234);
        
        PolyOperationConsumerProxy consumerProxy = new PolyOperationConsumerProxy(router);
        ConsumerSuperType result = consumerProxy.invokeOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter, result);
    }

    /**
     * Test case: The invocation of a method with parameter and result structures that contain polymorphic values works as expected.
     */
    @Test
    void containedPolymorphicTypes() {
        PolyOperation2ProviderProxy providerProxy = new PolyOperation2ProviderProxy();
        RequestRouter router = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerSubTypeA element = new ConsumerSubTypeA();
        element.setFieldA("Test");
        
        ConsumerStructureWithPolyField parameter = new ConsumerStructureWithPolyField();
        parameter.setField(element);
        
        PolyOperation2ConsumerProxy consumerProxy = new PolyOperation2ConsumerProxy(router);
        ConsumerStructureWithPolyField result = consumerProxy.invokeOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter, result);
    }
    
    /**
     * Test case: A thrown exception is mapped as expected.
     */
    @Test
    void exceptionMapping() {
        OpWithExceptionProviderProxy providerProxy = new OpWithExceptionProviderProxy();
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        
        OpWithExceptionConsumerProxy consumerProxy = new OpWithExceptionConsumerProxy(requestRouter);
        ConsumerTestException exception = assertThrows(ConsumerTestException.class, () -> consumerProxy.invokeOperation(new ConsumerParameter()));
        
        assertEquals(1234, exception.getExceptionField());
    }
    
    /**
     * Test case: Mono-to-poly mapping (parameter) and vice versa (result).
     */
    @Test
    void monoToPolyTypeMapping() {
        MonoToPolyMappingProviderProxy providerProxy = new MonoToPolyMappingProviderProxy();
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerMonoToPolyType parameter = new ConsumerMonoToPolyType();
        parameter.setField1(1234);
        
        MonoToPolyMappingConsumerProxy consumerProxy = new MonoToPolyMappingConsumerProxy(requestRouter);
        ConsumerMonoToPolyType result = consumerProxy.invokeOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter, result);
    }
    
    /**
     * Mono-to-poly type mapping where a specialized type is returned that is unknown to the consumer.
     * This does not lead to an error, as the type can be mapped to the formal type.
     */
    @Test
    void monoToPolyTypeMappingWithSpecializedResult() {
        MonoToPolyMappingProviderProxy providerProxy = new MonoToPolyMappingProviderProxy();
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        
        ConsumerMonoToPolyType parameter = new ConsumerMonoToPolyType();
        parameter.setField1(1);
        
        MonoToPolyMappingConsumerProxy consumerProxy = new MonoToPolyMappingConsumerProxy(requestRouter);
        ConsumerMonoToPolyType result = consumerProxy.invokeOperation(parameter);
        
        assertEquals(5678, result.getField1());
    }
    
    /**
     * Test case: Mono-to-poly mapping (and vice versa) as part of structure mapping.
     */
    @Test
    void embeddedMonoToPolyTypeMapping() {
        EmbeddedMonoToPolyMappingProviderProxy providerProxy = new EmbeddedMonoToPolyMappingProviderProxy();
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
                
        ConsumerMonoToPolyType monoToPolyValue = new ConsumerMonoToPolyType();
        monoToPolyValue.setField1(1234);
        
        ConsumerStructureWithMonoToPolyField parameter = new ConsumerStructureWithMonoToPolyField();
        parameter.setField(monoToPolyValue);
        
        EmbeddedMonoToPolyMappingConsumerProxy consumerProxy = new EmbeddedMonoToPolyMappingConsumerProxy(requestRouter);
        ConsumerStructureWithMonoToPolyField result = consumerProxy.invokeOperation(parameter);
        
        assertNotSame(parameter, result);
        assertEquals(parameter, result);
    }
    
    /**
     * Test case: The provider throws an exception, but the consumer does not expect one. This results in an unrepresentable value.
     */
    @Test
    void providerThrowsExceptionButConsumerDoesNotExpectOne() {
        OpWithUnmappedExceptionProviderProxy providerProxy = new OpWithUnmappedExceptionProviderProxy();
        RequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
   
        OpWithUnmappedExceptionConsumerProxy consumerProxy = new OpWithUnmappedExceptionConsumerProxy(requestRouter);
        assertThrows(UnrepresentableValueException.class, () -> consumerProxy.invokeOperation(new ConsumerParameter()));
    }

    private abstract static class ConsumerOperationProxyTemplate<P, R> extends ConsumerOperationProxy<P, R> {
                
        private static final String API_ID = "apis/consumer-api.api";

        private static final String REFERENCED_API_NAME = "test.provider";

        private static final int REFERENCED_REVISION = 0;
        
        private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath(API_ID, REFERENCED_API_NAME, REFERENCED_REVISION);
        
        protected ConsumerOperationProxyTemplate(String operationName, String parameterTypeName, String resultTypeName, Class<R> resultType, RequestRouter router) {
            super(CONSUMER_API, API_ID, operationName, parameterTypeName, resultTypeName, resultType, router);
        }
        
        protected ConsumerOperationProxyTemplate(String operationName, String parameterTypeName, String resultTypeName, Class<R> resultType, Set<Class<?>> exceptionTypes, RequestRouter router) {
            super(CONSUMER_API, API_ID, operationName, parameterTypeName, resultTypeName, resultType, exceptionTypes, router);
        }
        
    }
    
    /**
     * Template type for provider operation proxies.
     * @param <P> The parameter type of the operation
     * @param <R> The result type of the operation
     */
    private abstract static class ProviderOperationProxyTemplate<P, R> extends ProviderOperationProxy<P, R> {
        
        private static final RevisionHistory REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api");
        
        private static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1));
        
        protected ProviderOperationProxyTemplate(String operationName, String parameterTypeName, String resultTypeName, Class<P> parameterType) {
            super(operationName, REVISION_HISTORY, SUPPORTED_REVISIONS, parameterTypeName, resultTypeName, parameterType);
        }
        
    }
    
    private static class TestOperationConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerParameter, ConsumerResult> {
        
        public TestOperationConsumerProxy(RequestRouter router) {
            super("testOperation", "ConsumerParameter", "ConsumerResult", ConsumerResult.class, router);
        }
        
    }
    
    private static class TestOperationProviderProxy extends ProviderOperationProxyTemplate<ProviderParameter, ProviderResult> {

        public TestOperationProviderProxy() {
            super("testOperation", "ProviderParameter", "ProviderResult", ProviderParameter.class);
        }
        
        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            ProviderResult result = new ProviderResult();
            result.setRetField(parameter.getFieldA() + "X");
            result.setResultEnum(ProviderEnum.VALUE_2);

            List<ProviderEnum> resultList = new ArrayList<>(parameter.getTestList());
            Collections.reverse(resultList);
            result.setResultList(resultList);

            return result;
        }
        
    }
    
    private static class PolyOperationConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerSuperType, ConsumerSuperType> {
        
        public PolyOperationConsumerProxy(RequestRouter router) {
            super("polyOperation", "ConsumerSuperType", "ConsumerSuperType", ConsumerSuperType.class, router);
        }
        
    }
    
    private static class PolyOperationProviderProxy extends ProviderOperationProxyTemplate<ProviderSuperType, ProviderSuperType> {
        
        public PolyOperationProviderProxy() {
            super("polyOperation", "ProviderSuperType", "ProviderSuperType", ProviderSuperType.class);
        }
        
        @Override
        protected ProviderSuperType invokeOperation(ProviderSuperType parameter) {
            return parameter;
        }
        
    }
    
    private static class PolyOperation2ConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerStructureWithPolyField, ConsumerStructureWithPolyField> {
        
        public PolyOperation2ConsumerProxy(RequestRouter router) {
            super("polyOperation2", "ConsumerStructureWithPolyField", "ConsumerStructureWithPolyField", ConsumerStructureWithPolyField.class, router);
        }
        
    }
    
    private static class PolyOperation2ProviderProxy extends ProviderOperationProxyTemplate<ProviderStructureWithPolyField, ProviderStructureWithPolyField> {
        
        public PolyOperation2ProviderProxy() {
            super("polyOperation2", "ProviderStructureWithPolyField", "ProviderStructureWithPolyField", ProviderStructureWithPolyField.class);
        }
        
        @Override
        protected ProviderStructureWithPolyField invokeOperation(ProviderStructureWithPolyField parameter) {
            return parameter;
        }
        
    }
    
    private static class OpWithExceptionConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerParameter, ConsumerResult> {
        
        public OpWithExceptionConsumerProxy(RequestRouter router) {
            super("opWithException", "ConsumerParameter", "ConsumerResult", ConsumerResult.class, Collections.singleton(ConsumerTestExceptionData.class), router);
        }
        
    }
    
    private static class OpWithExceptionProviderProxy extends ProviderOperationProxyTemplate<ProviderParameter, ProviderResult> {
        
        public OpWithExceptionProviderProxy() {
            super("opWithException", "ProviderParameter" , "ProviderResult", ProviderParameter.class);
        }
        
        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            ProviderTestException exceptionData = new ProviderTestException();
            exceptionData.setExceptionField(1234);
            
            throw new MappableProviderTestException(exceptionData);
        }
        
    }
    
    private static class OpWithUnmappedExceptionConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerParameter, ConsumerResult> {
        
        public OpWithUnmappedExceptionConsumerProxy(RequestRouter router) {
            super("opWithUnmappedException", "ConsumerParameter", "ConsumerResult", ConsumerResult.class, Collections.emptySet(), router);
        }
        
    }
    
    private static class OpWithUnmappedExceptionProviderProxy extends ProviderOperationProxyTemplate<ProviderParameter, ProviderResult> {
        
        public OpWithUnmappedExceptionProviderProxy() {
            super("opWithUnmappedException", "ProviderParameter" , "ProviderResult", ProviderParameter.class);
        }
        
        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            ProviderTestException exceptionData = new ProviderTestException();
            exceptionData.setExceptionField(1234);
            
            throw new MappableProviderTestException(exceptionData);
        }
        
    }
    
    private static class MonoToPolyMappingConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerMonoToPolyType, ConsumerMonoToPolyType> {
        
        public MonoToPolyMappingConsumerProxy(RequestRouter router) {
            super("monoToPolyMapping", "ConsumerMonoToPolyType", "ConsumerMonoToPolyType", ConsumerMonoToPolyType.class, router);
        }
        
    }
    
    private static class MonoToPolyMappingProviderProxy extends ProviderOperationProxyTemplate<ProviderMonoToPolyType, ProviderMonoToPolyType> {
        
        public MonoToPolyMappingProviderProxy() {
            super("monoToPolyMapping", "ProviderMonoToPolyType", "ProviderMonoToPolyType", ProviderMonoToPolyType.class);
        }
        
        @Override
        protected ProviderMonoToPolyType invokeOperation(ProviderMonoToPolyType parameter) {
            if (parameter.getField1() == 1) {
                ProviderMonoToPolySubTypeA result = new ProviderMonoToPolySubTypeA();
                result.setField1(5678);
                result.setField2(4321);
                
                return result;                
            } else {
                ProviderMonoToPolyType result = new ProviderMonoToPolyType();
                result.setField1(1234);
                
                return result;
            }
        }
        
    }

    private static class EmbeddedMonoToPolyMappingConsumerProxy extends ConsumerOperationProxyTemplate<ConsumerStructureWithMonoToPolyField, ConsumerStructureWithMonoToPolyField> {
        
        public EmbeddedMonoToPolyMappingConsumerProxy(RequestRouter router) {
            super("embeddedMonoToPolyMapping", "ConsumerStructureWithMonoToPolyField", "ConsumerStructureWithMonoToPolyField", ConsumerStructureWithMonoToPolyField.class, router);
        }
        
    }
    
    private static class EmbeddedMonoToPolyMappingProviderProxy extends ProviderOperationProxyTemplate<ProviderStructureWithMonoToPolyField, ProviderStructureWithMonoToPolyField> {
        
        public EmbeddedMonoToPolyMappingProviderProxy() {
            super("embeddedMonoToPolyMapping", "ProviderStructureWithMonoToPolyField", "ProviderStructureWithMonoToPolyField", ProviderStructureWithMonoToPolyField.class);
        }
        
        @Override
        protected ProviderStructureWithMonoToPolyField invokeOperation(ProviderStructureWithMonoToPolyField parameter) {
            return parameter;
        }
        
    }
    
}
