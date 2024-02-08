package gutta.apievolution.jmh.fixedformat;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.customerexample.fixedformat.consumer.v1.Address;
import gutta.apievolution.customerexample.fixedformat.consumer.v1.Customer;
import gutta.apievolution.customerexample.fixedformat.consumer.v1.UpsertOperationConsumerProxyV1;
import gutta.apievolution.customerexample.fixedformat.provider.UpsertOperationProviderProxy;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

public class FixedFormatCustomerExampleBenchmarksV1 extends FixedFormatCustomerExampleBenchmarkTemplate {

    private static final FixedFormatMapper MAPPER = new FixedFormatMapper();

    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY,
            SUPPORTED_REVISIONS, CONSUMER_API_V1);

    private static final ApiMappingScript CONSUMER_TO_PROVIDER_SCRIPT = new ApiMappingScriptGenerator().generateMappingScript(DEFINITION_RESOLUTION,
            MappingDirection.CONSUMER_TO_PROVIDER);

    private static final ApiMappingScript PROVIDER_TO_CONSUMER_SCRIPT = new ApiMappingScriptGenerator().generateMappingScript(DEFINITION_RESOLUTION,
            MappingDirection.PROVIDER_TO_CONSUMER);

    private static final RequestRouter REQUEST_ROUTER = new RequestRouter(
            new UpsertOperationProviderProxy(CONSUMER_TO_PROVIDER_SCRIPT, PROVIDER_TO_CONSUMER_SCRIPT, MAPPER, CHARSET));

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

    private static final UpsertOperationConsumerProxyV1 CONSUMER_PROXY = new UpsertOperationConsumerProxyV1(REQUEST_ROUTER, MAPPER, CHARSET);

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeFromV1Client() {
        CONSUMER_PROXY.invoke(CUSTOMER);
    }

}
