package gutta.apievolution.benchmarks.json;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.customerexample.json.consumer.v6.Customer;
import gutta.apievolution.customerexample.json.consumer.v6.Gender;
import gutta.apievolution.customerexample.json.consumer.v6.StreetAddress;
import gutta.apievolution.customerexample.json.consumer.v6.UpsertOperationConsumerProxyV6;
import gutta.apievolution.customerexample.json.provider.UpsertOperationProviderProxy;
import gutta.apievolution.json.RequestRouter;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class JSONCustomerExampleBenchmarksV6 extends JSONCustomerExampleBenchmarkTemplate {
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY,
            SUPPORTED_REVISIONS, CONSUMER_API_V6);
    
    private static final Customer CUSTOMER = createCustomer();

    private static Customer createCustomer() {
        StreetAddress address = new StreetAddress();
        address.setStreet("Test Street");
        address.setNumber(1234);
        address.setCity("Test City");
        address.setPostalCode(5678);

        Customer customer = new Customer();
        customer.setFirstName("Test");
        customer.setLastName("Test");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(Gender.THIRD);
        customer.setPrimaryAddress(address);

        return customer;
    }

    private static final RequestRouter ROUTER = createRouter();

    private static RequestRouter createRouter() {
        return new JsonRequestRouter(DEFINITION_RESOLUTION, new UpsertOperationProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS));
    }
    
    private static final UpsertOperationConsumerProxyV6 CONSUMER_PROXY = new UpsertOperationConsumerProxyV6(CONSUMER_API_V6, CONSUMER_API_ID_V6, ROUTER);

    @Benchmark
    public void invokeFromV6Client_short() {
        CONSUMER_PROXY.invokeOperation(CUSTOMER);
    }
    
}
