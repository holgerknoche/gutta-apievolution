package gutta.apievolution.jmh.json;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.customerexample.json.consumer.v3.Address;
import gutta.apievolution.customerexample.json.consumer.v3.Customer;
import gutta.apievolution.customerexample.json.consumer.v3.UpsertOperationConsumerProxyV3;
import gutta.apievolution.customerexample.json.provider.UpsertOperationProviderProxy;
import gutta.apievolution.json.RequestRouter;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

public class JSONCustomerExampleBenchmarksV3 extends JSONCustomerExampleBenchmarkTemplate {
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY,
            SUPPORTED_REVISIONS, CONSUMER_API_V3);
    
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
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(0);
        customer.setPrimaryAddress(address);

        return customer;
    }

    private static final RequestRouter ROUTER = createRouter();

    private static RequestRouter createRouter() {
        return new JsonRequestRouter(DEFINITION_RESOLUTION, new UpsertOperationProviderProxy(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS));
    }
    
    private static final UpsertOperationConsumerProxyV3 CONSUMER_PROXY = new UpsertOperationConsumerProxyV3(CONSUMER_API_V3, CONSUMER_API_ID_V3, ROUTER);

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeFromV3Client() {
        CONSUMER_PROXY.invokeOperation(CUSTOMER);
    }
    
}
