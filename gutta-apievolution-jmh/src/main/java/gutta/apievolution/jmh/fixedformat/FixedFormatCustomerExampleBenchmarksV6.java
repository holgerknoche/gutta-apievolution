package gutta.apievolution.jmh.fixedformat;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.customerexample.fixedformat.consumer.v6.Customer;
import gutta.apievolution.customerexample.fixedformat.consumer.v6.Gender;
import gutta.apievolution.customerexample.fixedformat.consumer.v6.StreetAddress;
import gutta.apievolution.customerexample.fixedformat.consumer.v6.UpsertOperationConsumerProxyV6;
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

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class FixedFormatCustomerExampleBenchmarksV6 extends FixedFormatCustomerExampleBenchmarkTemplate {
    
    private static final FixedFormatMapper MAPPER = new FixedFormatMapper();
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY,
            SUPPORTED_REVISIONS, CONSUMER_API_V6);
    
    private static final ApiMappingScript CONSUMER_TO_PROVIDER_SCRIPT = new ApiMappingScriptGenerator().generateMappingScript(DEFINITION_RESOLUTION,
            MappingDirection.CONSUMER_TO_PROVIDER);
    
    private static final ApiMappingScript PROVIDER_TO_CONSUMER_SCRIPT = new ApiMappingScriptGenerator().generateMappingScript(DEFINITION_RESOLUTION,
            MappingDirection.PROVIDER_TO_CONSUMER);

    private static final RequestRouter REQUEST_ROUTER = new RequestRouter(new UpsertOperationProviderProxy(CONSUMER_TO_PROVIDER_SCRIPT, PROVIDER_TO_CONSUMER_SCRIPT, MAPPER, CHARSET));

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
    
    private static final UpsertOperationConsumerProxyV6 CONSUMER_PROXY = new UpsertOperationConsumerProxyV6(REQUEST_ROUTER, MAPPER, CHARSET);
    
    @Benchmark
    public void invokeFromV6Client_short() {
        CONSUMER_PROXY.invoke(CUSTOMER);
    }    

}
