package gutta.apievolution.json.unrepresentablevalues;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.RequestRouter;
import gutta.apievolution.json.SimpleJsonRequestRouter;
import gutta.apievolution.json.consumer.ConsumerOperationProxy;
import gutta.apievolution.json.consumer.OnUnrepresentableValue;
import gutta.apievolution.json.consumer.UnrepresentableValueException;
import gutta.apievolution.json.provider.ProviderOperationProxy;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class UnrepresentableValuesTest {
    
    private static final String API_ID = "apis/unrepresentablevalues/consumer-api.api";
    
    private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath(API_ID, "test.provider", 0);

    private static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/unrepresentablevalues/provider-revision-1.api", "apis/unrepresentablevalues/provider-revision-2.api"
            );

    private static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1));        
        
    @Test
    void unrepresentableSubtype() {
        UnrepresentableSubtypeProviderProxy providerProxy = new UnrepresentableSubtypeProviderProxy("SuperType", "SuperType", ProviderSuperType.class);        
        SimpleJsonRequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter);

        assertThrows(UnrepresentableValueException.class, () -> consumerProxy.invokeOperation(new ConsumerSuperType()));    	
    }
    
    @Test
    void unrepresentableSubtypeAsNull() {
    	UnrepresentableSubtypeProviderProxy providerProxy = new UnrepresentableSubtypeProviderProxy("SuperType", "SuperType", ProviderSuperType.class);        
        SimpleJsonRequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter);

        assertNull(consumerProxy.invokeOperation(new ConsumerSuperType(), OnUnrepresentableValue.returnNull()));
    }
    
    @Test
    void unrepresentableEnumMember() {
    	UnrepresentableEnumMemberProviderProxy providerProxy = new UnrepresentableEnumMemberProviderProxy("SuperType", "SuperType", ProviderSuperType.class);
    	SimpleJsonRequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
    	TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter);
    	
    	assertThrows(UnrepresentableValueException.class, () -> consumerProxy.invokeOperation(new ConsumerSuperType()));
    }
    
    @Test
    void unrepresentableEnumMemberAsNull() {
    	UnrepresentableEnumMemberProviderProxy providerProxy = new UnrepresentableEnumMemberProviderProxy("SuperType", "SuperType", ProviderSuperType.class);
    	SimpleJsonRequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
    	TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter);
    	
    	ConsumerSubTypeA result = (ConsumerSubTypeA) consumerProxy.invokeOperation(new ConsumerSuperType(), OnUnrepresentableValue.returnNull());
    	assertEquals(1234, result.getSubValueA());
    	assertNull(result.getEnumValue());
    }
    
    private static class UnrepresentableSubtypeProviderProxy extends ProviderOperationProxy<ProviderSuperType, ProviderSuperType> {

        public UnrepresentableSubtypeProviderProxy(String parameterTypeName, String resultTypeName, Class<ProviderSuperType> parameterType) {            
            super("op", PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, parameterTypeName, resultTypeName, parameterType);
        }

        @Override
        public ProviderSuperType invokeOperation(ProviderSuperType parameter) {
            ProviderSubTypeB subTypeB = new ProviderSubTypeB();
            subTypeB.setSubValueB("Test");
            
            return subTypeB;
        }
        
    }
    
    private static class UnrepresentableEnumMemberProviderProxy extends ProviderOperationProxy<ProviderSuperType, ProviderSuperType> {
    	
    	public UnrepresentableEnumMemberProviderProxy(String parameterTypeName, String resultTypeName, Class<ProviderSuperType> parameterType) {
    		super("op", PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, parameterTypeName, resultTypeName, parameterType);
    	}

		@Override
		protected ProviderSuperType invokeOperation(ProviderSuperType parameter) {
			ProviderSubTypeA subTypeA = new ProviderSubTypeA();
			subTypeA.setSubValueA(1234);
			subTypeA.setEnumValue(ProviderEnumeration.VALUE_B);
			
			return subTypeA;
		}
    	
    }
    
    private static class TestOperationConsumerProxy extends ConsumerOperationProxy<ConsumerSuperType, ConsumerSuperType> {

        public TestOperationConsumerProxy(RequestRouter requestRouter) {
            super(CONSUMER_API, API_ID, "op", "SuperType", "SuperType", ConsumerSuperType.class, requestRouter);
        }
                
    }
	
}
