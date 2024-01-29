package gutta.apievolution.json.unrepresentablevalues;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.ProviderOperationProxy;
import gutta.apievolution.json.RequestRouter;
import gutta.apievolution.json.SimpleJsonRequestRouter;
import gutta.apievolution.json.UnrepresentableValueException;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

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
        TestOperationProviderProxy providerProxy = new TestOperationProviderProxy("SuperType", "SuperType", ProviderSuperType.class);        
        SimpleJsonRequestRouter requestRouter = new SimpleJsonRequestRouter(providerProxy);
        TestOperationConsumerProxy consumerProxy = new TestOperationConsumerProxy(requestRouter);

        assertThrows(UnrepresentableValueException.class, () -> consumerProxy.invokeOperation(new ConsumerSuperType()));    	
    }
    
    private static class TestOperationProviderProxy extends ProviderOperationProxy<ProviderSuperType, ProviderSuperType> {

        public TestOperationProviderProxy(String parameterTypeName, String resultTypeName, Class<ProviderSuperType> parameterType) {
            
            super("op", PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, parameterTypeName, resultTypeName, parameterType);
        }

        @Override
        public ProviderSuperType invokeOperation(ProviderSuperType parameter) {
            ProviderSubTypeB subTypeB = new ProviderSubTypeB();
            subTypeB.setSubValueB("Test");
            
            return subTypeB;
        }
        
    }
    
    private static class TestOperationConsumerProxy extends ConsumerOperationProxy<ConsumerSuperType, ConsumerSuperType> {

        public TestOperationConsumerProxy(RequestRouter requestRouter) {
            super(CONSUMER_API, API_ID, "op", "SuperType", "SuperType", ConsumerSuperType.class, requestRouter);
        }
                
    }
	
}
