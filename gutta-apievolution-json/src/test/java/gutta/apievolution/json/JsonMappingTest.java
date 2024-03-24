package gutta.apievolution.json;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.consumer.ConsumerEnum;
import gutta.apievolution.json.consumer.ConsumerParameter;
import gutta.apievolution.json.consumer.ConsumerResult;
import gutta.apievolution.json.consumer.ConsumerStructureWithPolyField;
import gutta.apievolution.json.consumer.ConsumerSubTypeA;
import gutta.apievolution.json.consumer.ConsumerSubTypeB;
import gutta.apievolution.json.consumer.ConsumerSuperType;
import gutta.apievolution.json.provider.ProviderEnum;
import gutta.apievolution.json.provider.ProviderParameter;
import gutta.apievolution.json.provider.ProviderResult;
import gutta.apievolution.json.provider.ProviderStructureWithPolyField;
import gutta.apievolution.json.provider.ProviderSuperType;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.fail;

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
        consumerProxy.invokeOperation(new ConsumerParameter());
        fail();
    }

    private abstract static class ConsumerOperationProxyTemplate<P, R> extends ConsumerOperationProxy<P, R> {
                
        private static final String API_ID = "apis/consumer-api.api";

        private static final String REFERENCED_API_NAME = "test.provider";

        private static final int REFERENCED_REVISION = 0;
        
        private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath(API_ID, REFERENCED_API_NAME, REFERENCED_REVISION);
        
        protected ConsumerOperationProxyTemplate(String operationName, String parameterTypeName, String resultTypeName, Class<R> resultType, RequestRouter router) {
            super(CONSUMER_API, API_ID, operationName, parameterTypeName, resultTypeName, resultType, router);
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
            super("opWithException", "ConsumerParameter", "ConsumerResult", ConsumerResult.class, router);
        }
        
    }
    
    private static class OpWithExceptionProviderProxy extends ProviderOperationProxyTemplate<ProviderParameter, ProviderResult> {
        
        public OpWithExceptionProviderProxy() {
            super("opWithException", "ProviderParameter" , "ProviderResult", ProviderParameter.class);
        }
        
        @Override
        protected ProviderResult invokeOperation(ProviderParameter parameter) {
            // TODO Auto-generated method stub
            return null;
        }
        
    }

}
