package gutta.apievolution.benchmarks.json;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.customerexample.json.consumer.v1.Address;
import gutta.apievolution.customerexample.json.consumer.v1.Customer;
import gutta.apievolution.customerexample.json.consumer.v1.UpsertOperationConsumerProxyV1;
import gutta.apievolution.customerexample.json.provider.UpsertOperationProviderProxy;
import gutta.apievolution.json.RequestRouter;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class JSONCustomerExampleBenchmarksV1 extends JSONCustomerExampleBenchmarkTemplate {
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY,
            SUPPORTED_REVISIONS, CONSUMER_API_V1);
    
    private static final Customer CUSTOMER = createCustomer();

    private static Customer createCustomer() {
        Address address = new Address();
        address.setStreet("Test Street");
        address.setNumber(1234);
        address.setCity("Test City");
        address.setPostalCode(5678);

        Customer customer = new Customer();
        customer.setFirstName("Test");
        customer.setLastName("Test");
        customer.setGender(0);
        customer.setAddress(address);

        return customer;
    }

    private static final RequestRouter ROUTER = createRouter();

    private static RequestRouter createRouter() {
        return new JsonRequestRouter(DEFINITION_RESOLUTION, new UpsertOperationProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS));
    }
    
    private static final UpsertOperationConsumerProxyV1 CONSUMER_PROXY = new UpsertOperationConsumerProxyV1(CONSUMER_API_V1, CONSUMER_API_ID_V1, ROUTER);

    @Benchmark
    public void invokeFromV1Client_short() {
        CONSUMER_PROXY.invokeOperation(CUSTOMER);
    }
    
}
