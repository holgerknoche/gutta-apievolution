package gutta.apievolution.jmh.inprocess.dynproxy;

import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6.ConsumerApi;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6.Customer;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6.CustomerImpl;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6.Gender;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6.StreetAddress;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6.StreetAddressImpl;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class DynProxyCustomerExampleBenchmarksV6 extends CustomerExampleDynProxyBenchmarkTemplate {

    private static final Customer CUSTOMER = createCustomer();
    
    private static Customer createCustomer() {
        StreetAddress address = new StreetAddressImpl();
        address.setStreet("Test Street");
        address.setNumber(1234);
        address.setCity("Test City");
        address.setPostalCode(5678);

        Customer customer = new CustomerImpl();
        customer.setFirstName("Test");
        customer.setLastName("Test");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(Gender.THIRD);
        customer.setPrimaryAddress(address);

        return customer;
    }
    
    private static final ConsumerApi CONSUMER_API = createApi(ConsumerApi.class, CONSUMER_API_V6);
    
    @Benchmark
    public void invokeFromV6Client_short() {
        CONSUMER_API.upsert(CUSTOMER);
    }
    
}
